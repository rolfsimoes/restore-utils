#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix C_validation_compare_versions(NumericMatrix data, NumericMatrix mask, int data_class, int mask_class) {
    int npixel = data.nrow();

    if (data.nrow() != mask.nrow()) {
        stop("Data and mask must have the same dimensions");
    }

    for (int i = 0; i < npixel; i++) {
        // Compare values
        bool is_valid_in_current_version = data(i, 0) == data_class;
        bool is_valid_in_another_version = mask(i, 0) == mask_class;

        // Case 1: current version (data) is equal to another version (mask)
        if (is_valid_in_current_version && is_valid_in_another_version) {
            data(i, 0) = 1;

            continue;
        }

        // Case 2: current version (data) has a value that is not in another version (mask)
        if (is_valid_in_current_version && !is_valid_in_another_version) {
            data(i, 0) = 2;

            continue;
        }

        // Case 3: current version (data) does not have a value that is another version (mask)
        if (!is_valid_in_current_version && is_valid_in_another_version) {
            data(i, 0) = 3;

            continue;
        }

        // Case 4: Both versions are different
        if (!is_valid_in_current_version && !is_valid_in_another_version) {
            data(i, 0) = 4;

            continue;
        }

        // If it is not any other case, set to 99
        data(i, 0) = 99;
    }

    return data;
}
