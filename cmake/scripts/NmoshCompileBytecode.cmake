# Compile and create bytecode archive
#
# TARGET   = target os id
# NMOSH    = path to nmosh interpreter (guessed)
# LOADPATH = path to nmosh libraries(list)
# SRCPATH  = path to nmosh source
# OUTPUT   = path to FASL output

if(NOT NMOSH)
    find_program(NMOSH nmosh)
endif()
if(NOT TARGET)
    set(TARGET "HOST")
endif()

if(WIN32)
    set(pathsep ";")
else()
    set(pathsep ":")
endif()

macro(gen_loadpath var)
    set(_total "")
    foreach(e ${ARGN})
        set(_total "${_total}${pathsep}${e}")
    endforeach()
    set(${var} ${_total})
endmacro(gen_loadpath)

# Generate paths

set(runner ${SRCPATH}/boot/runtimes/srfi-mosh/run-fasl.scm)
set(script ${SRCPATH}/boot/runtimes/srfi-mosh/run-turbocharge.sps)
set(fasl ${SRCPATH}/src/nmosh_boot.fasl)
gen_loadpath(lpath ${LOADPATH})

execute_process(
    COMMAND ${NMOSH} -5 ${runner} ${fasl} ${lpath} ${TARGET} ${script} 
    "${OUTPUT}"
    )

