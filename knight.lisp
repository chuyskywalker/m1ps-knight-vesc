; This LISP file is designed to be installed on the STM (maxim) unit in the controller.
; A different file (str365.lisp) is installed on the ESP chip in the controller.
; Finally, installed the normal disp35b package, downloaded it, and injected a bit
; of code to listen for the `301` drive mode and update the drive mode for the display.

(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)

; track if the brake light should be on/off
(def brake-light-val 0)

(def drive-mode 1)

; I kinda wish there was a temporary version of this like there is l-current-max-scale
; Some motors may spin the opposite direction and ...
; wait, I don't do any conf-set/save/etc -- maybe I can read the current value at (main)
; and just invert based on that? Need to look into that...
(def m-direction-for-reverse 1)
(def m-direction-for-forward 0)

@const-start

(defun proc-sid (id data) {
    (cond
        ((= id 301) {
            (var drive-mode-new (bufget-u8 data 0))
            (if (!= drive-mode-new drive-mode) {
                (print (list "setting drive mode from -> to " drive-mode drive-mode-new))
                (setq drive-mode drive-mode-new)
                (match drive-mode
                (0 { ; Reverse
                    (conf-set 'l-current-max-scale 0.2)
                    (conf-set 'max-speed (/ 5.0 3.6))
                    (conf-set 'm-invert-direction m-direction-for-reverse)
                })
                (1 { ; Neutral
                    (conf-set 'l-current-max-scale 0.0)
                    (conf-set 'max-speed 0)
                    (conf-set 'm-invert-direction m-direction-for-forward)
                })
                (2 { ; 1
                    (conf-set 'l-current-max-scale 0.3)
                    (conf-set 'max-speed (/ 12.0 3.6))
                    (conf-set 'm-invert-direction m-direction-for-forward)
                })
                (3 { ; 2
                    (conf-set 'l-current-max-scale 0.6)
                    (conf-set 'max-speed (/ 25.0 3.6))
                    (conf-set 'm-invert-direction m-direction-for-forward)
                })
                (4 { ; 3
                    (conf-set 'l-current-max-scale 1.0)
                    (conf-set 'max-speed (/ 200.0 3.6))
                    (conf-set 'm-invert-direction m-direction-for-forward)
                })
                )
            })

        })
    )
})

(defun event-handler () {
    (loopwhile t
        (recv
            ((event-can-sid . ((? id) . (? data))) (proc-sid id data))
            (_ nil)
})))


(defun main () {
    (set-print-prefix "STM-")

    (print "KNIGHT ACTIVE")

    ; turn on the 12v
    (set-aux 1 1)

    ; setup loop for handling incoming can messages
    (event-register-handler (spawn event-handler))
    (event-enable 'event-can-sid)

    ; start a thread to report stats that the display will use
    ; this is more-or-less a copy/paste from
    ;   https://github.com/vedderb/vesc_pkg/blob/af658dd56a7f9d26c8f38361f42d9eef563d34ce/dash35b/main-esc.lisp#L133
    ; with the exception that I removed the dual motor stuff
    (var buf-can (array-create 8))
    (loopwhile-thd ("Stats" 200) t {
        (bufclear buf-can)
        (bufset-i16 buf-can 0 (* (get-batt) 1000))
        (bufset-i16 buf-can 2 (* (abs (get-duty)) 1000))
        (bufset-i16 buf-can 4 (* (abs (get-speed)) 3.6 10))
        (can-send-sid 20 buf-can)

        (bufclear buf-can)
        (if (> (get-bms-val 'bms-temp-adc-num) 2)
            (bufset-i16 buf-can 0 (* (get-bms-val 'bms-temps-adc 2) 10))
            (bufset-i16 buf-can 0 0)
        )
        (bufset-i16 buf-can 2 (* (get-temp-fet) 10))
        (bufset-i16 buf-can 4 (* (get-temp-mot) 10))
        (bufset-i16 buf-can 6 (* (ix (get-imu-rpy) 1) 100))
        (can-send-sid 21 buf-can)

        (bufclear buf-can)
        (bufset-u16 buf-can 0 (* (get-wh) 10.0))
        (bufset-u16 buf-can 2 (* (get-wh-chg) 10.0))
        (bufset-u16 buf-can 4 (* (/ (get-dist-abs) 1000.0) 10))
        (bufset-u16 buf-can 6 (get-fault))
        (can-send-sid 22 buf-can)

        (bufclear buf-can)
        (bufset-u16 buf-can 0 (to-i (stats 'stat-current-avg)))
        (bufset-u16 buf-can 2 (to-i (stats 'stat-current-max)))
        (bufset-i16 buf-can 4 (to-i (get-current)))
        (bufset-u16 buf-can 6 (to-i (conf-get 'si-battery-ah)))
        (can-send-sid 23 buf-can)

        (bufclear buf-can)
        (bufset-u16 buf-can 0 (* (get-vin) 10))
        (bufset-u32 buf-can 2 (* (/ (sysinfo 'odometer) 1000.0) 10))
        (bufset-u16 buf-can 6 (* (abs (get-speed-set)) 3.6 10)) ; Cruise control speed
        (can-send-sid 24 buf-can)

        (sleep 0.1)
    })

    ; main show!
    ; detach the brake control and flip it to regen when pulled
    ; also, when pulled, push out an event that the brake light should be on/off
    (app-adc-detach 2 1)

    (loopwhile-thd ("ADCWatch" 200) t {
        (if (> (get-adc 1) 1.0) {
            (app-adc-override 2 1.0)
            (setq brake-light-val 1)
        }{
            (app-adc-override 2 0.0)
            (setq brake-light-val 0)
        })
        (sleep 0.05)
    })

    ; repeatedly send the brake light state -- only sent "on change" it's possible
    ; that the ESP could miss the message and now the light isn't in the correct state
    ; until the next message. This repeat ensures state stays accurate.
    ; timer is set low (4 times per second) to reduce noise but still be reasonably quick.
    (loopwhile-thd ("brakelightmsg" 200) t {
        (can-send-sid 314 (list brake-light-val 0 0 0 0 0 0 0))
        (sleep 0.25)
    })

    (start-code-server)

})

@const-end

(image-save)
(main)
