; This code is designed to be installed on the ESP chip (str365.io) in the maxim controller
; Here we are going to read the buttons and turn on some lights.
; Currently, all of the lighting is done directly via switches on the bike; I did not feel like
; wiring up all of the lighting (and the blinking routines) through the VESC.
; This does, however, mean that the display can't mirror the current state of the bike (for instance,
; there is no headlight on/off, no blinkers, etc). I don't really consider it a huge downside, though.
; The system COULD be rewired, but meh for now.

(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)

(import "pkg@://vesc_packages/lib_tca9535/tca9535.vescpkg" 'tca9535)
(read-eval-program tca9535)

(def brake-light-status 0)

(def io-pin-park 13)
(def io-pin-mode 15)

(define last-park-state 0)
(define last-mode-state 0)

(def drive-mode 1) ; 0 reverse; 1 neutral; 2 low; 3 med; 4 high

; For each "mode", what should the mode be when the button is pressed?
;                       0   1   2   3   4
;                       R>N N>R L>N M>N H>N
(define park-mappings [ 1   0   1   1   1  ])

;                       0   1   2   3   4
;                       R>L N>L L>M M>H H>L
(define mode-mappings [ 2   2   3   4   2  ])

@const-start

(defun proc-sid (id data) {
    (cond
        ((= id 314) {
            (setq brake-light-status (bufget-u8 data 0))
            (gpio-write 3 brake-light-status) ; a bit "unsafe" in that the event could have any int here, but whatever
        })
    )
})

(defun event-handler ()
    (loopwhile t
        (recv
            ((event-can-sid . ((? id) . (? data))) (proc-sid id data))
            (_ nil)
)))

(defun main () {
    (set-print-prefix "ESC-")

    ; https://github.com/vedderb/vesc_pkg/blob/af658dd56a7f9d26c8f38361f42d9eef563d34ce/vl_bike_39p/code_esp.lbm#L174
    ; Force ublox-driver to stop as we will use the same pins for the io-expander
    (uart-start 0 20 21 115200)
    (uart-stop 0)
    ; //end copypaste

    ; just make sure the light is in OUTPUT mode
    (gpio-configure 3 'pin-mode-out)

    (event-register-handler (spawn event-handler))
    (event-enable 'event-can-sid)

    (start-code-server)

    (tca9535-init 0x20 'rate-100k 21 20)
    (tca9535-set-dir '(17 out))
    ; set the tca to read high pins, since that's where the park/mode
    ; button are and I'm not using the other pins
    (tca9535-write-pins '(17 1))

    (loopwhile-thd ("Stats" 200) t {

        (var pin-states (tca9535-read-pins io-pin-park io-pin-mode))
        (var park-state (ix pin-states 0))
        (var mode-state (ix pin-states 1))

        ;(def drive-mode 1) ; 0 reverse; 1 neutral; 2 low; 3 med; 4 high
        (if (and (= last-mode-state 1) (= mode-state 0)) {
            (setq drive-mode (ix mode-mappings drive-mode))
            ; send out the new drive mode
            (can-send-sid 301 (list drive-mode 0 0 0 0 0 0 0))
        })

        (if (and (= last-park-state 1) (= park-state 0)) {
            (setq drive-mode (ix park-mappings drive-mode))
            ; send out the new drive mode
            (can-send-sid 301 (list drive-mode 0 0 0 0 0 0 0))
        })

        (setq last-mode-state mode-state)
        (setq last-park-state park-state)

        (sleep 0.1) ; a relatively quick loop to catch the button press
                    ; kinda wish there was an interrupt driven version of this
    })

})

@const-end

(image-save)
(main)