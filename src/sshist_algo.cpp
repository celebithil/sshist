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
