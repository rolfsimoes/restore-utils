#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix C_trajectory_transition_analysis(NumericMatrix data, int reference_class, int neighbor_class) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear != 10) {
        stop("Expected exactly 10 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        for (int j = 0; j + 2 < nyear; j += 3) {

            bool valid_neihbor = data(i, j) == neighbor_class && data(i, j + 2) == neighbor_class;
            bool valid_class = data(i, j + 1) == reference_class;

            if (valid_class && valid_neihbor) {
                data(i, j + 1) = neighbor_class;

                // 9 means the last time step
                // 6 means the last iteration for the moving window
                if (j == 6 && data(i, 9) == reference_class) {
                    data(i, nyear - 1) = neighbor_class;
                }
            }
        }
    }

    return data;
}


// [[Rcpp::export]]
NumericMatrix C_trajectory_neighbor_analysis(NumericMatrix data, int reference_class, int replacement_class) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    if (nyear < 3) {
        stop("Expected at least 3 years (columns), but got " + std::to_string(nyear));
    }

    for (int i = 0; i < npixel; i++) {
        // remove edges (start: j = 1; end = j - 1)
        for (int j = 1; j < nyear - 1; j++) {

            if (data(i, j) == reference_class) {
                bool is_left_diff = data(i, j - 1) != reference_class;
                bool is_right_diff = data(i, j + 1) != reference_class;

                if (is_left_diff && is_right_diff) {
                    data(i, j) = replacement_class;
                }
            }
        }
    }

    return data;
}


// [[Rcpp::export]]
NumericMatrix C_trajectory_urban_analysis(NumericMatrix data, NumericMatrix mask, int urban_class_id, int forest_class_id, int forest_class_id_mask) {
    int npixel = data.nrow();
    int nyear = data.ncol();

    for (int i = 0; i < npixel; i++) {
        int forest_index = -1;
        int urban_index = -1;
        int urban_change = 0;

        // Looking for urban in the series
        for (int j = 0; j < nyear; j++) {
            if (data(i, j) == urban_class_id) {
                urban_index = j;
            }
        }

        // If urban exist, index will not be `-1`
        if (urban_index != -1) {

            // Looking for changes in the time-series
            for (int j = urban_index; j < nyear; j++) {
                if (urban_change) {
                    break;
                }

                if (data(i, j) != urban_class_id) {
                    urban_change = 1;

                    break;
                }
            }

        }

        // Check if change goes to forest
        if (urban_change) {
            // Looking for forest
            for (int j = urban_index; j < nyear; j++) {
                // If forest was already identified, skip operation
                if (forest_index != -1) {
                    break;
                }

                // If is a forest, save the index
                if (data(i, j) == forest_class_id) {
                    forest_index = j;

                    break;
                }
            }
        }

        // If we change the firest index, this means we identify forest
        // in a urban area time-series
        if (forest_index != -1) {

            // Transforming everything in forest
            for (int j = urban_index; j < nyear; j++) {
                if (mask(i, j) == forest_class_id_mask) {
                    data(i, j) = forest_class_id;
                }
            }

        } else if (urban_index != -1) {
            // Transforming everything in urban area
            for (int j = urban_index; j < nyear; j++) {
                data(i, j) = urban_class_id;
            }
        }
    }

    return data;
}


