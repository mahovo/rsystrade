
#include <Rcpp.h>
using namespace Rcpp;

// Helper to compute default block length if passed as -1
int compute_default_block_length(NumericMatrix x) {
  int n = x.nrow();
  NumericVector ac1 = NumericVector(x.ncol());
  for (int j = 0; j < x.ncol(); ++j) {
    double mu = mean(x(_, j));
    double num = 0.0, denom = 0.0;
    for (int t = 1; t < n; ++t) {
      num += (x(t, j) - mu) * (x(t - 1, j) - mu);
      denom += (x(t, j) - mu) * (x(t, j) - mu);
    }
    if (denom != 0.0) {
      ac1[j] = std::abs(num / denom);
    } else {
      ac1[j] = 0.0;
    }
  }
  double rho1 = mean(ac1);
  int candidate = std::floor(10.0 / (1.0 - rho1));
  return std::max(5, std::min(candidate, (int)std::sqrt(n)));
}

// [[Rcpp::export]]
List blockBootstrap(NumericMatrix x, int n_length = -1, int block_length = -1, int num_blocks = -1, int num_boots = 20,
                    std::string block_type = "stationary", double p = 0.1, bool overlap = true) {

  const int n = x.nrow();    // Number of observations (time points)
  const int d = x.ncol();    // Number of variables (columns)

  if (block_length == -1) {
    block_length = compute_default_block_length(x);
  }

  if (block_length > n) {
    stop("block_length must be less than or equal to the number of observations   (n).");
  }

  const int bl = block_length;

  // Handle n_length based on user input and num_blocks
  if (num_blocks > 0) {
    n_length = num_blocks * bl;
  } else if (n_length == -1) {
    n_length = n;
  }

  // Container to hold all bootstrap replicates
  List boots(num_boots);

  for (int b = 0; b < num_boots; ++b) {
    // Initialize matrix for this bootstrap sample
    // Maximum length = n_length (may be slightly less for stationary blocks with short last block)
    NumericMatrix sample(n_length, d);

    int pos = 0; // Current position in the bootstrap sample

    while (pos < n_length) {

      int start_idx;       // Starting index of block in original data (0-based)
      int current_block_len;  // Length of the block to copy

      if (block_type == "moving") {
        // --- Moving Block Bootstrap ---
        // Fixed block length = block_length
        // Choose start index uniformly from [0, n - bl]
        start_idx = (int) std::floor(R::runif(0, n - bl + 1));
        current_block_len = bl;
      } else if (block_type == "stationary") {
        // --- Stationary Bootstrap ---
        // Block length is random: geometric(p) + 1, capped by bl
        current_block_len = (int) R::rgeom(p) + 1;
        if (current_block_len > bl) {
          current_block_len = bl;
        }

        if (overlap) {
          start_idx = (int) std::floor(R::runif(0, n));
        } else {
          int max_start = (n / bl) * bl;
          int num_starts = max_start / bl;
          int block_num = (int) std::floor(R::runif(0, num_starts));
          start_idx = block_num * bl;
        }
      } else {
        stop("Unsupported block_type. Use 'moving' or 'stationary'.");
      }

      // Copy rows from x to sample
      int rows_to_copy = std::min(current_block_len, num_blocks * bl - pos);

      for (int i = 0; i < rows_to_copy; ++i) {
        int idx = start_idx + i;
        if (idx >= n) {
          idx = idx % n;
        }
        for (int col = 0; col < d; ++col) {
          sample(pos + i, col) = x(idx, col);
        }
      }

      pos += rows_to_copy;
    }

    boots[b] = sample;
  }

  return boots;
}
