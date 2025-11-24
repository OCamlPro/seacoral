#include "cbmc_init.h"

#ifdef CBMC_COVER_MODE
#include "cbmc_cover_driver.h"
#elifdef CBMC_ASSERT_MODE
#include "cbmc_assert_driver.h"
#elifdef CBMC_CLABEL_MODE
#include "cbmc_label_driver.h"
#endif
