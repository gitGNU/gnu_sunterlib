;;; scgamedictionaries.scm - a scheme dictionary system for scgame 
;;;
;;; Copyright (c) 2011-2012 Johan Ceuppens
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;; Dictionary ADT with ref,set!,add!,make public methods at the end

(define (make-dictionary1)
  ;; methods are FIFO (first fixed first out)  
  (let ((*dict '()))

    (define (get key) ;; get key
      (do ((l *dict (cdr l)))
	  ((eq? key (caar l))
	   (cadr l));;returns value
	))
    (define (add key value)
      (set! *dict (append *dict (list (list key value)))))

    (define (set key value) ;; get key
      (do ((l *dict (cdr l))
	   (res '() (append (list (car l) res))))
	  ((eq? key (caar l))
	   (set! (cadr res) value)
	   (set! *dict (append res (cdr l))))
	))

    
    (lambda (msg)
      (cond ((eq? msg 'get) get)
	    ((eq? msg 'set) set)	    
	    ((eq? msg 'add) add)
	    (else (aspecterror)(display "make-dictionary"))))
    ))

(define (make-dictionary) (make-dictionary1))
(define (dictionary-ref dict key) ((dict 'get) key))
(define (dictionary-set! dict key value) ((dict 'set) key value))
(define (dictionary-add! dict key value) ((dict 'add) key value))

;; color dictionary - for CSS colors use bpp set to 0
;; helper functions

(define (string->color str)
  (let ((colornumber 0))
    (do ((i 0 (+ i 1)))
	((< i (string-length str)) colornumber)
      (let* ((c (string-ref str i))
	     (n (cond ((or (eq? c #\a)(eq? c #\A))
		       10)
		      ((or (eq? c #\b)(eq? c #\B))
		       11)
		      ((or (eq? c #\c)(eq? c #\C))
		       12)
		      ((or (eq? c #\d)(eq? c #\D))
		       13)
		      ((or (eq? c #\e)(eq? c #\E))
		       14)
		      ((or (eq? c #\f)(eq? c #\F))
		       15)
		      (else (string->number (string c))))))
	(set! colornumber (+ (* n 16) colornumber))))))

(define (little-endian->big-endian n)
  (let ((str (string n))
	(rets ""))
    (do ((i (string-length str) (- i 1)))
	((<= i 0)
	 (string->number rets))
      (set! rets (string 
		  (bitwise-and 
		   (string->number (* (expt 2 i)(string-ref str i)))
		   (string->number rets)))))
    ))

(define (big-endian->littleendian n)
  (let ((str (string n))
	(rets ""))
    (do ((i 0 (+ i 1)))
	((>= i (string-length str))
	 (string->number rets))
      (set! rets (string 
		  (bitwise-and 
		   (string->number (* (expt 2 i)(string-ref str i)))
		   (string->number rets)))))
    ))


(define (make-color-dictionary bpp)
  (let ((dict (make-dictionary))
	(pow (expt 2 bpp)))
    (cond ((= pow 16) ;; 16 colors 
	   (do ((i 0 (+ i 1)))
	       ((< i pow) 
		(dictionary-add! dict i i)))
	   (dictionary-add! dict 'black 0)

	   ;; ... FIXME fill in 4-bit colors
	   dict)
	  ((= pow 256) ;; 256 colors
	   (dictionary-add! dict 'black 0)
	   ;; ... FIXME fill in 8-bit colors
	   dict)
	  (else (display "color-dictionary : no or unsupported bit depth. Using CSS dictionary")	  
		(dictionary-add! dict 'Black   "000000")
		(dictionary-add! dict 'Navy   "000080")
		(dictionary-add! dict 'DarkBlue   "00008B")
		(dictionary-add! dict 'MediumBlue   "0000CD")
		(dictionary-add! dict 'Blue   "0000FF")
		(dictionary-add! dict 'DarkGreen   "006400")
		(dictionary-add! dict 'Green   "008000")
		(dictionary-add! dict 'Teal   "008080")
		(dictionary-add! dict 'DarkCyan   "008B8B")
		(dictionary-add! dict 'DeepSkyBlue   "00BFFF")
		(dictionary-add! dict 'DarkTurquoise   "00CED1")
		(dictionary-add! dict 'MediumSpringGreen   "00FA9A")
		(dictionary-add! dict 'Lime   "00FF00")
		(dictionary-add! dict 'SpringGreen   "00FF7F")
		(dictionary-add! dict 'Aqua   "00FFFF")
		(dictionary-add! dict 'Cyan   "00FFFF")
		(dictionary-add! dict 'MidnightBlue   "191970")
		(dictionary-add! dict 'DodgerBlue   "1E90FF")
		(dictionary-add! dict 'LightSeaGreen   "20B2AA")
		(dictionary-add! dict 'ForestGreen   "228B22")
		(dictionary-add! dict 'SeaGreen   "2E8B57")
		(dictionary-add! dict 'DarkSlateGray   "2F4F4F")
		(dictionary-add! dict 'DarkSlateGrey   "2F4F4F")
		(dictionary-add! dict 'LimeGreen   "32CD32")
		(dictionary-add! dict 'MediumSeaGreen   "3CB371")
		(dictionary-add! dict 'Turquoise   "40E0D0")
		(dictionary-add! dict 'RoyalBlue   "4169E1")
		(dictionary-add! dict 'SteelBlue   "4682B4")
		(dictionary-add! dict 'DarkSlateBlue   "483D8B")
		(dictionary-add! dict 'MediumTurquoise   "48D1CC")
		(dictionary-add! dict 'Indigo    "4B0082")
		(dictionary-add! dict 'DarkOliveGreen   "556B2F")
		(dictionary-add! dict 'CadetBlue   "5F9EA0")
		(dictionary-add! dict 'CornflowerBlue   "6495ED")
		(dictionary-add! dict 'MediumAquaMarine   "66CDAA")
		(dictionary-add! dict 'DimGray   "696969")
		(dictionary-add! dict 'DimGrey   "696969")
		(dictionary-add! dict 'SlateBlue   "6A5ACD")
		(dictionary-add! dict 'OliveDrab   "6B8E23")
		(dictionary-add! dict 'SlateGray   "708090")
		(dictionary-add! dict 'SlateGrey   "708090")
		(dictionary-add! dict 'LightSlateGray   "778899")
		(dictionary-add! dict 'LightSlateGrey   "778899")
		(dictionary-add! dict 'MediumSlateBlue   "7B68EE")
		(dictionary-add! dict 'LawnGreen   "7CFC00")
		(dictionary-add! dict 'Chartreuse   "7FFF00")
		(dictionary-add! dict 'Aquamarine   "7FFFD4")
		(dictionary-add! dict 'Maroon   "800000")
		(dictionary-add! dict 'Purple   "800080")
		(dictionary-add! dict 'Olive   "808000")
		(dictionary-add! dict 'Gray   "808080")
		(dictionary-add! dict 'Grey   "808080")
		(dictionary-add! dict 'SkyBlue   "87CEEB")
		(dictionary-add! dict 'LightSkyBlue   "87CEFA")
		(dictionary-add! dict 'BlueViolet   "8A2BE2")
		(dictionary-add! dict 'DarkRed   "8B0000")
		(dictionary-add! dict 'DarkMagenta   "8B008B")
		(dictionary-add! dict 'SaddleBrown   "8B4513")
		(dictionary-add! dict 'DarkSeaGreen   "8FBC8F")
		(dictionary-add! dict 'LightGreen   "90EE90")
		(dictionary-add! dict 'MediumPurple   "9370D8")
		(dictionary-add! dict 'DarkViolet   "9400D3")
		(dictionary-add! dict 'PaleGreen   "98FB98")
		(dictionary-add! dict 'DarkOrchid   "9932CC")
		(dictionary-add! dict 'YellowGreen   "9ACD32")
		(dictionary-add! dict 'Sienna   "A0522D")
		(dictionary-add! dict 'Brown   "A52A2A")
		(dictionary-add! dict 'DarkGray   "A9A9A9")
		(dictionary-add! dict 'DarkGrey   "A9A9A9")
		(dictionary-add! dict 'LightBlue   "ADD8E6")
		(dictionary-add! dict 'GreenYellow   "ADFF2F")
		(dictionary-add! dict 'PaleTurquoise   "AFEEEE")
		(dictionary-add! dict 'LightSteelBlue   "B0C4DE")
		(dictionary-add! dict 'PowderBlue   "B0E0E6")
		(dictionary-add! dict 'FireBrick   "B22222")
		(dictionary-add! dict 'DarkGoldenRod   "B8860B")
		(dictionary-add! dict 'MediumOrchid   "BA55D3")
		(dictionary-add! dict 'RosyBrown   "BC8F8F")
		(dictionary-add! dict 'DarkKhaki   "BDB76B")
		(dictionary-add! dict 'Silver   "C0C0C0")
		(dictionary-add! dict 'MediumVioletRed   "C71585")
		(dictionary-add! dict 'IndianRed    "CD5C5C")
		(dictionary-add! dict 'Peru   "CD853F")
		(dictionary-add! dict 'Chocolate   "D2691E")
		(dictionary-add! dict 'Tan   "D2B48C")
		(dictionary-add! dict 'LightGray   "D3D3D3")
		(dictionary-add! dict 'LightGrey   "D3D3D3")
		(dictionary-add! dict 'PaleVioletRed   "D87093")
		(dictionary-add! dict 'Thistle   "D8BFD8")
		(dictionary-add! dict 'Orchid   "DA70D6")
		(dictionary-add! dict 'GoldenRod   "DAA520")
		(dictionary-add! dict 'Crimson   "DC143C")
		(dictionary-add! dict 'Gainsboro   "DCDCDC")
		(dictionary-add! dict 'Plum   "DDA0DD")
		(dictionary-add! dict 'BurlyWood   "DEB887")
		(dictionary-add! dict 'LightCyan   "E0FFFF")
		(dictionary-add! dict 'Lavender   "E6E6FA")
		(dictionary-add! dict 'DarkSalmon   "E9967A")
		(dictionary-add! dict 'Violet   "EE82EE")
		(dictionary-add! dict 'PaleGoldenRod   "EEE8AA")
		(dictionary-add! dict 'LightCoral   "F08080")
		(dictionary-add! dict 'Khaki   "F0E68C")
		(dictionary-add! dict 'AliceBlue   "F0F8FF")
		(dictionary-add! dict 'HoneyDew   "F0FFF0")
		(dictionary-add! dict 'Azure   "F0FFFF")
		(dictionary-add! dict 'SandyBrown   "F4A460")
		(dictionary-add! dict 'Wheat   "F5DEB3")
		(dictionary-add! dict 'Beige   "F5F5DC")
		(dictionary-add! dict 'WhiteSmoke   "F5F5F5")
		(dictionary-add! dict 'MintCream   "F5FFFA")
		(dictionary-add! dict 'GhostWhite   "F8F8FF")
		(dictionary-add! dict 'Salmon   "FA8072")
		(dictionary-add! dict 'AntiqueWhite   "FAEBD7")
		(dictionary-add! dict 'Linen   "FAF0E6")
		(dictionary-add! dict 'LightGoldenRodYellow   "FAFAD2")
		(dictionary-add! dict 'OldLace   "FDF5E6")
		(dictionary-add! dict 'Red   "FF0000")
		(dictionary-add! dict 'Fuchsia   "FF00FF")
		(dictionary-add! dict 'Magenta   "FF00FF")
		(dictionary-add! dict 'DeepPink   "FF1493")
		(dictionary-add! dict 'OrangeRed   "FF4500")
		(dictionary-add! dict 'Tomato   "FF6347")
		(dictionary-add! dict 'HotPink   "FF69B4")
		(dictionary-add! dict 'Coral   "FF7F50")
		(dictionary-add! dict 'Darkorange   "FF8C00")
		(dictionary-add! dict 'LightSalmon   "FFA07A")
		(dictionary-add! dict 'Orange   "FFA500")
		(dictionary-add! dict 'LightPink   "FFB6C1")
		(dictionary-add! dict 'Pink   "FFC0CB")
		(dictionary-add! dict 'Gold   "FFD700")
		(dictionary-add! dict 'PeachPuff   "FFDAB9")
		(dictionary-add! dict 'NavajoWhite   "FFDEAD")
		(dictionary-add! dict 'Moccasin   "FFE4B5")
		(dictionary-add! dict 'Bisque   "FFE4C4")
		(dictionary-add! dict 'MistyRose   "FFE4E1")
		(dictionary-add! dict 'BlanchedAlmond   "FFEBCD")
		(dictionary-add! dict 'PapayaWhip   "FFEFD5")
		(dictionary-add! dict 'LavenderBlush   "FFF0F5")
		(dictionary-add! dict 'SeaShell   "FFF5EE")
		(dictionary-add! dict 'Cornsilk   "FFF8DC")
		(dictionary-add! dict 'LemonChiffon   "FFFACD")
		(dictionary-add! dict 'FloralWhite   "FFFAF0")
		(dictionary-add! dict 'Snow   "FFFAFA")
		(dictionary-add! dict 'Yellow   "FFFF00")
		(dictionary-add! dict 'LightYellow   "FFFFE0")
		(dictionary-add! dict 'Ivory   "FFFFF0")
		(dictionary-add! dict 'White   "FFFFFF")
		))
    
    ))
