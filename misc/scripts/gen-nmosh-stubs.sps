(import (rnrs)
        (shorten)
        (nmosh internal buildsystem gen-stubs))

(repository-generate (make-repository '("src/nmosh")) 
                     "src/call-stubs.inc.c"
                     "src/embed-libs.inc.h"
                     "src/embed-libs.inc.c")
