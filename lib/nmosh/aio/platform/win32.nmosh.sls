(library (nmosh aio platform win32)
         (export ;; core functions
                 queue
                 queue-dispose
                 queue-wait/timeout
                 queue-wait
                 queue-peek
                 queue-dispatch
                 
                 queue-invoke-ffithread

                 queue-write0
                 queue-read0
                 queue-close0
                 queue-spawn0
                 queue-open-serialport0
                 resolve-socketname/4
                 resolve-socketname/6
                 inetname-port
                 queue-listen
                 queue-accept
                 queue-connect
                 queue-window-register
                 queue-window-destroy
                 )
         (import (rnrs)
                 (nmosh aio impl win32 handle-ops)
                 (nmosh aio impl win32 queue-iocp)
                 (nmosh aio impl win32 socket-ops)
                 (nmosh aio impl win32 process-ops)
                 )



)
