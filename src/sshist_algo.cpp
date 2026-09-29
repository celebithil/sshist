#define R_NO_REMAP 1
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

// Cost function calculation (Parallel OpenMP)
//
// Internal function to calculate the cost function for the Shimazaki-Shinomoto method.
// Used by the high-level R function sshist().
//
// param x Numeric vector of data
// param N_vector Integer vector of bin counts to test
// param sn Integer number of shifts for averaging
// param x_min Double minimum value of data
// param x_max Double maximum value of data
// return Numeric vector of cost values
// [[Rcpp::export]]
NumericVector sshist_cost_cpp(NumericVector x, IntegerVector N_vector,
                              int sn, double x_min, double x_max,
                              int n_threads) {

  // 1. Data conversion to std::vector (Thread-safe)
  // R objects (NumericVector) are not thread-safe and cannot be safely accessed
  // directly inside an OpenMP parallel region.
  // We copy them to std::vector for 100% thread-safe reading.
  std::vector<double> x_vec = Rcpp::as<std::vector<double>>(x);
  std::vector<int> N_vec = Rcpp::as<std::vector<int>>(N_vector);

  int n_candidates = N_vec.size();
  int n_data = x_vec.size();

  // Vector to store the results (Average Cost for each N)
  std::vector<double> C_avg_vec(n_candidates);

  // 2. Parallel loop over candidate N values
  // We use schedule(dynamic) because the workload varies significantly:
  // larger N means more bins to process, taking more time.
#pragma omp parallel for schedule(dynamic) num_threads(n_threads)
  for(int i = 0; i < n_candidates; i++) {

    int N = N_vec[i];
    double D = (x_max - x_min) / N;// Bin width

    // Local binning (instead of calling R's hist())
    // bin counts vector
    std::vector<int> counts(N);
    std::vector<double> edges(N + 1);
    // Accumulator for the cost function across shifts
    double C_sum = 0.0;

    // Shift averaging loop (usually sn = 30)
    for(int s = 0; s < sn; s++) {
      std::fill(counts.begin(), counts.end(), 0);

      // PYTHON: np.linspace(0, D, sn)
      double shift = (sn > 1) ? (D / (sn - 1.0)) * s : 0.0;

      // PYTHON: np.linspace(start, end_edge, N+1)
      double A = x_min + shift - D/2.0;
      double B = x_max + shift - D/2.0;
      for(int e = 0; e <= N; e++) {
        edges[e] = A + (B - A) * e / N; // Formula without accumulating floating-point error
      }

      int count_sum = 0;
      // Iterate through data points
      for(int k = 0; k < n_data; k++) {
        double val = x_vec[k];

        // Check if value is within the shifted range
        // PYTHON: np.histogram (half-open intervals [a, b) and closed last interval [a, b])
        if (val >= edges[0] && val <= edges[N]) {
          // Binary search instead of division saves from errors like 0.999999999 -> 0
          auto it = std::upper_bound(edges.begin(), edges.end(), val);
          int bin_idx = std::distance(edges.begin(), it) - 1;

          // Boundary correction: handle the maximum value falling exactly on the upper edge
          if (bin_idx == N) bin_idx = N - 1; // Last bin includes the right edge

          if (bin_idx >= 0 && bin_idx < N) {
            counts[bin_idx]++;
            count_sum++;
          }
        }
      }

      // Edge case protection: if total count is zero (theoretically impossible here but good for safety)
      if (count_sum == 0) continue;

      // Calculate L2 Cost Function
      double k_mean = (double)count_sum / N;
      double v_sum = 0.0;

      for(int b = 0; b < N; b++) {
        double diff = counts[b] - k_mean;
        v_sum += diff * diff;
      }

      // Biased variance (division by N), consistent with Shimazaki & Shinomoto (2007)
      double k_var = v_sum / N;

      // Formula: (2*mean - variance) / width^2
      double C = (2.0 * k_mean - k_var) / (D * D);

      C_sum += C;
    }

    // Average cost over all shifts
    C_avg_vec[i] = C_sum / sn;
  }

  // Convert result back to R vector
  return Rcpp::wrap(C_avg_vec);
}

// 1. Fast search for min/max distances for W bounds (zero RAM overhead)
// [[Rcpp::export]]
NumericVector get_tau_bounds_cpp(NumericVector xn, NumericVector yn,
                                 int n_threads) {
  int N = xn.size();
  double min_tau = R_PosInf;
  double max_tau = 0.0;

  // Use raw pointers for rapid, safe access
  double* p_xn = xn.begin();
  double* p_yn = yn.begin();

#ifdef _OPENMP
#pragma omp parallel for schedule(static) reduction(min:min_tau) reduction(max:max_tau) num_threads(n_threads)
#endif
  for (int i = 0; i < N - 1; i++) {
    double xi = p_xn[i];
    double yi = p_yn[i];
    for (int j = i + 1; j < N; j++) {
      double dx = p_xn[j] - xi;
      double dy = p_yn[j] - yi;
      double tau = dx*dx + dy*dy;
      if (tau > 2.220446e-16) { // Protection against division by zero (machine eps)
        if (tau < min_tau) min_tau = tau;
      }
      if (tau > max_tau) max_tau = tau;
    }
  }
  return NumericVector::create(min_tau, max_tau);
}

// 2. Fast calculation of the Cost Function for 2D KDE
// [[Rcpp::export]]
double compute_sskernel2d_cost_cpp(NumericVector xn, NumericVector yn, double w,
                                   int n_threads) {
  int N = xn.size();
  double term = 0.0;
  double four_w2 = 4.0 * w * w;
  double two_w2 = 2.0 * w * w;

  double* p_xn = xn.begin();
  double* p_yn = yn.begin();

#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic) reduction(+:term) num_threads(n_threads)
#endif
  for (int i = 0; i < N - 1; i++) {
    double xi = p_xn[i];
    double yi = p_yn[i];
    double local_term = 0.0;
    for (int j = i + 1; j < N; j++) {
      double dx = p_xn[j] - xi;
      double dy = p_yn[j] - yi;
      double tau = dx*dx + dy*dy;
      if (tau > 2.220446e-16) {
        local_term += exp(-tau / four_w2) - 4.0 * exp(-tau / two_w2);
      }
    }
    term += local_term;
  }

  double C_val = ((double)N / (w*w)) + (2.0 / (w*w)) * term;
  return C_val / (4.0 * M_PI);
}

// 3. Computation of pilot density for the adaptive method
// [[Rcpp::export]]
NumericVector compute_pilot_density_cpp(NumericVector x, NumericVector y, double wx, double wy,
                                        int n_threads) {
  int N = x.size();
  NumericVector pilot_density(N);

  double* p_x = x.begin();
  double* p_y = y.begin();
  double* p_out = pilot_density.begin(); // Raw pointer for thread safety

  double norm_const = 1.0 / (2.0 * M_PI * wx * wy);
  double two_wx2 = 2.0 * wx * wx;
  double two_wy2 = 2.0 * wy * wy;

#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads)
#endif
  for (int i = 0; i < N; i++) {
    double sum = 0.0;
    double xi = p_x[i];
    double yi = p_y[i];
    for (int j = 0; j < N; j++) {
      double dx = p_x[j] - xi;
      double dy = p_y[j] - yi;
      sum += exp(-(dx*dx / two_wx2 + dy*dy / two_wy2));
    }
    p_out[i] = norm_const * (sum / (double)N);
  }
  return pilot_density;
}

// 4. Final grid evaluation (Fixed and Adaptive)
// [[Rcpp::export]]
NumericMatrix compute_kde2d_cpp(NumericVector x, NumericVector y,
                                NumericVector gx, NumericVector gy,
                                NumericVector wx, NumericVector wy,
                                int n_threads) {
  int nx = gx.size();
  int ny = gy.size();
  int N = x.size();

  NumericMatrix Z(nx, ny);

  double* p_x = x.begin();
  double* p_y = y.begin();
  double* p_gx = gx.begin();
  double* p_gy = gy.begin();
  double* p_wx = wx.begin();
  double* p_wy = wy.begin();
  double* p_Z = Z.begin(); // Raw pointer to avoid Rcpp proxy overhead

  bool adaptive = (wx.size() > 1); // Check if window is fixed or adaptive
  double w_x = p_wx[0];
  double w_y = p_wy[0];

#ifdef _OPENMP
#pragma omp parallel for collapse(2) schedule(static) num_threads(n_threads)
#endif
  for(int i = 0; i < nx; i++) {
    for(int j = 0; j < ny; j++) {
      double sum = 0.0;
      for(int k = 0; k < N; k++) {
        double cur_wx = adaptive ? p_wx[k] : w_x;
        double cur_wy = adaptive ? p_wy[k] : w_y;

        double dx = (p_gx[i] - p_x[k]) / cur_wx;
        double dy = (p_gy[j] - p_y[k]) / cur_wy;

        sum += exp(-0.5 * (dx*dx + dy*dy)) / (2.0 * M_PI * cur_wx * cur_wy);
      }
      // R matrices are strictly column-major: Index is row + col * n_rows
      p_Z[i + j * nx] = sum / (double)N;
    }
  }

  return Z;
}

// =========================================================================
// ssvkernel: Multi-start GS-based gamma optimization (C++/OpenMP)
// =========================================================================

inline double kernel_boxcar(double d, double w) {
  double a = std::sqrt(12.0) * w;
  return (std::abs(d) <= a / 2.0) ? (1.0 / a) : 0.0;
}

inline double kernel_laplace(double d, double w) {
  return std::exp(-std::sqrt(2.0) * std::abs(d) / w) / (std::sqrt(2.0) * w);
}

inline double kernel_cauchy(double d, double w) {
  double r = d / w;
  return 1.0 / (M_PI * w * (1.0 + r * r));
}

inline double kernel_gauss(double d, double w) {
  return std::exp(-0.5 * d * d / (w * w)) / (std::sqrt(2.0 * M_PI) * w);
}

struct CostResult {
  double Cg;
  std::vector<double> yv;
  std::vector<double> optwp;
};

static CostResult compute_cost(
    const std::vector<double>& y_hist, int N,
    const std::vector<double>& t, double dt,
    const std::vector<double>& optws_gs, int M, int L,
    const std::vector<double>& WIN,
    const std::string& WinFunc, double g,
    const std::vector<double>& dist_mat,
    const std::vector<int>& idx_nz, int n_nz) {

  // Step 1: determine optwv from GS ratios
  std::vector<double> optwv(L);
  for (int j = 0; j < L; j++) {
    int base = j * M;
    double max_gs = 0.0;
    double min_gs = 2.0;
    int last_idx = -1;
    for (int i = 0; i < M; i++) {
      double gs = optws_gs[base + i];
      if (gs > max_gs) max_gs = gs;
      if (gs < min_gs) min_gs = gs;
      if (gs >= g) last_idx = i;
    }
    if (g > max_gs) {
      optwv[j] = WIN[0];
    } else if (g < min_gs) {
      optwv[j] = WIN[M - 1];
    } else if (last_idx >= 0) {
      optwv[j] = g * WIN[last_idx];
    } else {
      optwv[j] = WIN[M - 1];
    }
  }

  // Step 2: Nadaraya-Watson smoothing (fused loop, no Z_mat allocation)
  std::vector<double> optwp(L);
  for (int i = 0; i < L; i++) {
    double row_sum = 0.0;
    double w_sum = 0.0;
    for (int j = 0; j < L; j++) {
      double d = dist_mat[j * L + i];
      double w = optwv[j] / g;
      double z;
      if (WinFunc == "Boxcar") {
        z = kernel_boxcar(d, w);
      } else if (WinFunc == "Laplace") {
        z = kernel_laplace(d, w);
      } else if (WinFunc == "Cauchy") {
        z = kernel_cauchy(d, w);
      } else { // Gauss
        z = kernel_gauss(d, w);
      }
      row_sum += z;
      w_sum += z * optwv[j];
    }
    optwp[i] = (row_sum > 0) ? (w_sum / row_sum) : optwv[i];
  }

  // Step 3: Balloon density (fused over non-zero bins)
  std::vector<double> yv(L, 0.0);
  for (int i = 0; i < L; i++) {
    double sd = optwp[i];
    double inv_sd = 1.0 / sd;
    double norm = inv_sd / std::sqrt(2.0 * M_PI);
    double half_inv_sd2 = -0.5 * inv_sd * inv_sd;
    for (int k = 0; k < n_nz; k++) {
      int j = idx_nz[k];
      double d = dist_mat[j * L + i];
      yv[i] += norm * std::exp(half_inv_sd2 * d * d) * y_hist[j] * dt;
    }
  }

  double sum_yv_dt = 0.0;
  for (int i = 0; i < L; i++) sum_yv_dt += yv[i] * dt;
  if (sum_yv_dt > 0) {
    double scale = static_cast<double>(N) / sum_yv_dt;
    for (int i = 0; i < L; i++) yv[i] *= scale;
  }

  // Step 4: Cg
  double Cg = 0.0;
  double norm_const = 2.0 / std::sqrt(2.0 * M_PI);
  for (int i = 0; i < L; i++) {
    double hi = y_hist[i];
    double cg = yv[i] * yv[i] - 2.0 * yv[i] * hi + (norm_const / optwp[i]) * hi;
    Cg += cg * dt;
  }

  return {Cg, std::move(yv), std::move(optwp)};
}

// [[Rcpp::export]]
Rcpp::List ssvkernel_optimize_gamma_cpp(
    Rcpp::NumericVector y_hist_r, int N,
    Rcpp::NumericVector t_r, double dt,
    Rcpp::NumericMatrix optws_r,
    Rcpp::NumericVector WIN_r,
    std::string WinFunc,
    Rcpp::NumericMatrix dist_mat_r,
    int n_threads) {

  int L = y_hist_r.length();
  int M = WIN_r.length();

  std::vector<double> y_hist(y_hist_r.begin(), y_hist_r.end());
  std::vector<double> t_vec(t_r.begin(), t_r.end());
  std::vector<double> WIN(WIN_r.begin(), WIN_r.end());
  std::vector<double> dist_mat(dist_mat_r.begin(), dist_mat_r.end());

  // optws: column-major, pre-divide by WIN for GS
  std::vector<double> optws_gs(M * L);
  for (int i = 0; i < M; i++) {
    double win_i = WIN[i];
    for (int j = 0; j < L; j++) {
      optws_gs[j * M + i] = optws_r(i, j) / win_i;
    }
  }

  // non-zero histogram indices
  std::vector<int> idx_nz;
  for (int j = 0; j < L; j++)
    if (y_hist[j] > 0) idx_nz.push_back(j);
  int n_nz = idx_nz.size();

  // cost lambda (thread-safe: captures const refs to std::vector)
  auto cost_fn = [&](double g) -> double {
    return compute_cost(y_hist, N, t_vec, dt, optws_gs, M, L, WIN,
                        WinFunc, g, dist_mat, idx_nz, n_nz).Cg;
  };

  double best_gamma;
  std::vector<double> best_yv, best_optwp;

  // unique sorted GS in (1e-6, 1-1e-6)
  std::vector<double> GS_all = optws_gs;
  std::sort(GS_all.begin(), GS_all.end());
  auto last = std::unique(GS_all.begin(), GS_all.end());
  GS_all.erase(last, GS_all.end());

  std::vector<double> GS_01;
  GS_01.reserve(GS_all.size());
  for (double v : GS_all)
    if (v > 1e-6 && v < 1.0 - 1e-6) GS_01.push_back(v);

  int n_GS = GS_01.size();
  int n_intervals = n_GS - 1;

  if (n_intervals < 2) {
    // Fallback: uniform 100pt grid + golden section
    std::vector<double> gamma_grid(100);
    std::vector<double> C_coarse(100);
    for (int k = 0; k < 100; k++)
      gamma_grid[k] = 1e-4 + (1.0 - 1e-4) * k / 99.0;

#pragma omp parallel for num_threads(n_threads)
    for (int k = 0; k < 100; k++)
      C_coarse[k] = cost_fn(gamma_grid[k]);

    int best_idx = std::min_element(C_coarse.begin(), C_coarse.end()) - C_coarse.begin();

    double a = (best_idx == 0) ? 1e-12 : gamma_grid[best_idx - 1];
    double b = (best_idx == 99) ? 1.0 : gamma_grid[best_idx + 1];

    const double phi = (std::sqrt(5.0) + 1.0) / 2.0;
    double c1 = (phi - 1.0) * a + (2.0 - phi) * b;
    double c2 = (2.0 - phi) * a + (phi - 1.0) * b;
    double f1 = cost_fn(c1), f2 = cost_fn(c2);

    for (int k = 0; k < 30; k++) {
      if (std::abs(b - a) <= 1e-5 * (std::abs(c1) + std::abs(c2)) && k > 2) break;
      if (f1 < f2) {
        b = c2; c2 = c1; c1 = (phi - 1.0) * a + (2.0 - phi) * b;
        f2 = f1; f1 = cost_fn(c1);
      } else {
        a = c1; c1 = c2; c2 = (2.0 - phi) * a + (phi - 1.0) * b;
        f1 = f2; f2 = cost_fn(c2);
      }
    }
    best_gamma = (f1 < f2) ? c1 : c2;

    auto res = compute_cost(y_hist, N, t_vec, dt, optws_gs, M, L, WIN,
                            WinFunc, best_gamma, dist_mat, idx_nz, n_nz);
    best_yv = std::move(res.yv);
    best_optwp = std::move(res.optwp);

  } else {
    // Multi-start via GS intervals
    int n_mids = n_intervals + 2;
    std::vector<double> mids(n_mids);
    mids[0] = GS_01[0] / 2.0;
    for (int k = 1; k <= n_intervals; k++)
      mids[k] = (GS_01[k - 1] + GS_01[k]) / 2.0;
    mids[n_mids - 1] = (GS_01[n_GS - 1] + 1.0) / 2.0;

    std::vector<double> C_mid(n_mids);
#pragma omp parallel for num_threads(n_threads)
    for (int k = 0; k < n_mids; k++)
      C_mid[k] = cost_fn(mids[k]);

    int K = std::min(5, n_mids);
    std::vector<int> top_idx(K);
    {
      std::vector<std::pair<double, int>> sorted(n_mids);
      for (int k = 0; k < n_mids; k++)
        sorted[k] = {C_mid[k], k};
      std::sort(sorted.begin(), sorted.end());
      for (int k = 0; k < K; k++)
        top_idx[k] = sorted[k].second;
    }

    best_gamma = 0.0;
    double best_C = std::numeric_limits<double>::infinity();
    std::vector<double> g_results(K);
    std::vector<double> C_results(K, std::numeric_limits<double>::infinity());

    int n_gs_threads = std::min(K, n_threads > 0 ? n_threads : 1);
#pragma omp parallel for num_threads(n_gs_threads)
    for (int k = 0; k < K; k++) {
      int idx = top_idx[k];
      double a, b;
      if (idx == 0) {
        a = 1e-8; b = GS_01[0];
      } else if (idx == n_mids - 1) {
        a = GS_01[n_GS - 1]; b = 1.0;
      } else {
        a = GS_01[idx - 1]; b = GS_01[idx];
      }

      if (b - a < 1e-12) continue;

      const double phi = (std::sqrt(5.0) + 1.0) / 2.0;
      double c1 = (phi - 1.0) * a + (2.0 - phi) * b;
      double c2 = (2.0 - phi) * a + (phi - 1.0) * b;
      double f1 = cost_fn(c1), f2 = cost_fn(c2);

      for (int iter = 0; iter < 30; iter++) {
        if (std::abs(b - a) <= 1e-5 * (std::abs(c1) + std::abs(c2)) && iter > 2) break;
        if (f1 < f2) {
          b = c2; c2 = c1; c1 = (phi - 1.0) * a + (2.0 - phi) * b;
          f2 = f1; f1 = cost_fn(c1);
        } else {
          a = c1; c1 = c2; c2 = (2.0 - phi) * a + (phi - 1.0) * b;
          f1 = f2; f2 = cost_fn(c2);
        }
      }
      g_results[k] = (f1 < f2) ? c1 : c2;
      C_results[k] = cost_fn(g_results[k]);
    }

    for (int k = 0; k < K; k++) {
      if (C_results[k] < best_C) {
        best_C = C_results[k];
        best_gamma = g_results[k];
      }
    }

    auto res = compute_cost(y_hist, N, t_vec, dt, optws_gs, M, L, WIN,
                            WinFunc, best_gamma, dist_mat, idx_nz, n_nz);
    best_yv = std::move(res.yv);
    best_optwp = std::move(res.optwp);
  }

  return Rcpp::List::create(
    Rcpp::Named("gamma")  = best_gamma,
    Rcpp::Named("yv")     = Rcpp::wrap(best_yv),
    Rcpp::Named("optwp")  = Rcpp::wrap(best_optwp)
  );
}


