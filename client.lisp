(in-package #:tls-server/mini-http2)

;;; taken and adapted from http2 package

(defun write-frame-header (vector start length type http-stream-id flags)
  "Write a frame header to VECTOR at position START.
"
;;; All frames begin with a fixed 9-octet header followed by a variable-
;;; length payload.
;;;
;;;  +-----------------------------------------------+
;;;  |                 Length (24)                   |
;;;  +---------------+---------------+---------------+
;;;  |   Type (8)    |   Flags (8)   |
;;;  +-+-------------+---------------+-------------------------------+
;;;  |R|                 Stream Identifier (31)                      |
;;;  +=+=============================================================+
;;;  |                   Frame Payload (0...)                      ...
;;;  +---------------------------------------------------------------+
;;;
;;; Length:  The length of the frame payload expressed as an unsigned
;;;    24-bit integer.  Values greater than 2^14 (16,384) MUST NOT be
;;;    sent unless the receiver has set a larger value for
;;;    SETTINGS_MAX_FRAME_SIZE.
;;;
;;;    The 9 octets of the frame header are not included in this value.
;;;
;;; Type:  The 8-bit type of the frame.  The frame type determines the
;;;    format and semantics of the frame.
;;;
;;; Flags:  An 8-bit field reserved for boolean flags specific to the
;;;    frame type.
;;;
;;;    Flags are assigned semantics specific to the indicated frame type.
;;;    Flags that have no defined semantics for a particular frame type
;;;    MUST be ignored and MUST be left unset (0x0) when sending.
;;;
;;; R: A reserved 1-bit field.  The semantics of this bit are undefined,
;;;    and the bit MUST remain unset (0x0) when sending and MUST be
;;;    ignored when receiving.
;;;
;;; Stream Identifier:  A stream identifier (see Section 5.1.1) expressed
;;;    as an unsigned 31-bit integer.  The value 0x0 is reserved for
;;;    frames that are associated with the connection as a whole as
;;;    opposed to an individual stream."
  (declare (type (unsigned-byte 24) length)
           (type (unsigned-byte 8) type flags)
  ;         (optimize speed)
           )
  (assert (< length 256) ()
          "This implementation does not support sending large frames")
  (buffer-with-changed-stream vector (print http-stream-id) start)
  (setf (aref vector start) 0)
  (setf (aref vector (incf start)) 0)
  (setf (aref vector (incf start)) length)
  (setf (aref vector (incf start)) type)
  (setf (aref vector (incf start)) flags))

(defvar *request-headers* #(130 135 132 1 134 160 228 29 19 157 9)
  "GET / localhost")
