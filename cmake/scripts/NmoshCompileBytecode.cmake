# Compile and create bytecode archive
#
# TARGET   = target os id
# NMOSH    = path to nmosh interpreter (guessed)
# LOADPATH = path to nmosh libraries(list)
# SRCPATH  = path to nmosh source
# OUTPUT   = path to FASL output

if(NOT NMOSH)
    find_program(NMOSH nmosh REQUIRED)
endif()
if(NOT NMOSH)
    message(FATAL_ERROR "NMOSH was not found")
endif()
if(NOT EXISTS ${NMOSH})
    message(FATAL_ERROR "NMOSH was invalid")
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
separate_arguments(LOADPATH) # FIXME: It does not allow spaces in build path
gen_loadpath(lpath ${LOADPATH})

message(STATUS "Running: ${NMOSH} -5 ${runner} ${fasl} ${lpath} ${TARGET} ${script} ${OUTPUT}")


execute_process(
    COMMAND ${NMOSH} -5 ${runner} ${fasl} "${lpath}" ${TARGET} ${script} 
    "${OUTPUT}"
    RESULT_VARIABLE _res
    )
message(STATUS "Result: ${_res}")
if(${_res} EQUAL 0)
    message(STATUS "${OUTPUT} generated.")
else()
    message(FATAL_ERROR "err")
endif()

