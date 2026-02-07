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
NumericVector sshist_cost_cpp(NumericVector x, IntegerVector N_vector, int sn, double x_min, double x_max) {

  // 1. Data conversion to std::vector (Thread-safe)
  // R objects (NumericVector) are not thread-safe and cannot be safely accessed
  // directly inside an OpenMP parallel region. We copy them to std::vector.
  std::vector<double> x_vec = Rcpp::as<std::vector<double>>(x);
  std::vector<int> N_vec = Rcpp::as<std::vector<int>>(N_vector);

  int n_candidates = N_vec.size();
  int n_data = x_vec.size();

  // Vector to store the results (Average Cost for each N)
  std::vector<double> C_avg_vec(n_candidates);

  // 2. Parallel loop over candidate N values
  // We use schedule(dynamic) because the workload varies significantly:
  // larger N means more bins to process, taking more time.
#pragma omp parallel for schedule(dynamic)
  for(int i = 0; i < n_candidates; i++) {

    int N = N_vec[i];
    double D = (x_max - x_min) / N; // Bin width

    // Local binning (instead of calling R's hist())
    // bin counts vector
    std::vector<int> counts(N);
    // Accumulator for the cost function across shifts
    double C_sum = 0.0;

    // Shift averaging loop (usually sn = 30)
    for(int s = 0; s < sn; s++) {
      std::fill(counts.begin(), counts.end(), 0);
      double shift = (D / sn) * s;
      double start = x_min + shift - D/2.0;
      double end_edge = x_max + shift - D/2.0; // Not strictly needed for logic, but good for reference

      int count_sum = 0;

      // Iterate through data points
      for(int k = 0; k < n_data; k++) {
        double val = x_vec[k];

        // Check if value is within the shifted range
        if (val >= start && val <= end_edge) {

          // Fast bin index calculation without explicitly creating edge vectors
          int bin_idx = std::floor((val - start) / D);

          // Boundary correction: handle the maximum value falling exactly on the upper edge
          if (bin_idx == N) bin_idx = N - 1;

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
      double C = (2 * k_mean - k_var) / (D * D);

      C_sum += C;
    }

    // Average cost over all shifts
    C_avg_vec[i] = C_sum / sn;
  }

  // Convert result back to R vector
  return Rcpp::wrap(C_avg_vec);
}
