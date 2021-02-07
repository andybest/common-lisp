(in-package #:coherent-noise/internal)

(u:define-constant +perlin/permutation+
    (let ((permutation #(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142
                         8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117
                         35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71
                         134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41
                         55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89
                         18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217
                         226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58
                         17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155
                         167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218
                         246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249
                         14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4
                         150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156
                         180))
          (p (make-array 512 :element-type 'u:ub8 :initial-element 0)))
      (replace p permutation :start1 0)
      (replace p permutation :start1 256)
      p)
  :test #'equalp)

;;; 1D Perlin

(u:fn-> %perlin-1d (ub8-512 f50) u:f32)
(declaim (inline %perlin-1d))
(defun %perlin-1d (table x)
  (declare (optimize speed))
  (flet ((grad (hash x)
           (let* ((h (logand hash 15))
                  (grad (1+ (logand h 7))))
             (if (zerop (logand h 8))
                 (* grad x)
                 (* (- grad) x)))))
    (u:mvlet* ((xi xf (truncate x))
               (xi (logand xi 255)))
      (float (* (u:lerp (interpolate/quintic xf)
                        (grad (aref table xi) xf)
                        (grad (aref table (1+ xi)) (1- xf)))
                0.25)
             1f0))))

;;; 2D Perlin

(u:fn-> %perlin-2d (ub8-512 f50 f50) u:f32)
(declaim (inline %perlin-2d))
(defun %perlin-2d (table x y)
  (declare (optimize speed))
  (flet ((grad (hash x y)
           (let* ((h (logand hash 7))
                  (u (if (< h 4) x y))
                  (v (if (< h 4) y x)))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (u:mvlet* ((xi xf (truncate x))
               (yi yf (truncate y))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (u (interpolate/quintic xf))
               (a (+ (aref table xi) yi))
               (b (+ (aref table (1+ xi)) yi)))
      (float
       (u:lerp (interpolate/quintic yf)
               (u:lerp u
                       (grad (lookup table (aref table a)) xf yf)
                       (grad (lookup table (aref table b)) (1- xf) yf))
               (u:lerp u
                       (grad (lookup table (aref table (1+ a))) xf (1- yf))
                       (grad (lookup table (aref table (1+ b))) (1- xf) (1- yf))))
       1f0))))

;;; 3D Perlin

(u:fn-> %perlin-3d (ub8-512 f50 f50 f50) u:f32)
(declaim (inline %perlin-3d))
(defun %perlin-3d (table x y z)
  (declare (optimize speed))
  (flet ((grad (hash x y z)
           (let* ((h (logand hash 15))
                  (u (if (< h 8) x y))
                  (v (case h
                       ((0 1 2 3) y)
                       ((12 14) x)
                       (t z))))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (u:mvlet* ((xi xf (truncate x))
               (yi yf (truncate y))
               (zi zf (truncate z))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (u (interpolate/quintic xf))
               (v (interpolate/quintic yf))
               (w (interpolate/quintic zf))
               (a (+ (aref table xi) yi))
               (b (+ (aref table (1+ xi)) yi)))
      (float
       (u:lerp
        w
        (u:lerp
         v
         (u:lerp u
                 (grad (lookup table zi a) xf yf zf)
                 (grad (lookup table zi b) (1- xf) yf zf))
         (u:lerp u
                 (grad (lookup table zi (1+ a)) xf (1- yf) zf)
                 (grad (lookup table zi (1+ b)) (1- xf) (1- yf) zf)))
        (u:lerp
         v
         (u:lerp u
                 (grad (lookup table (1+ zi) a) xf yf (1- zf))
                 (grad (lookup table (1+ zi) b) (1- xf) yf (1- zf)))
         (u:lerp u
                 (grad (lookup table (1+ zi) (1+ a)) xf (1- yf) (1- zf))
                 (grad (lookup table zi (1+ b)) (1- xf) (1- yf) (1- zf)))))
       1f0))))

;;; 4D Perlin

(u:fn-> %perlin-4d (ub8-512 f50 f50 f50 f50) u:f32)
(declaim (inline %perlin-4d))
(defun %perlin-4d (table x y z w)
  (declare (optimize speed))
  (flet ((grad (hash x y z w)
           (let* ((h (logand hash 31))
                  (u (if (< h 24) x y))
                  (v (if (< h 16) y z))
                  (w (if (< h 8) z w)))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))
                (if (zerop (logand h 4)) w (- w))))))
    (u:mvlet* ((xi xf (truncate x))
               (yi yf (truncate y))
               (zi zf (truncate z))
               (wi wf (truncate w))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (wi (logand wi 255))
               (xi1 (logand (1+ xi) 255))
               (yi1 (logand (1+ yi) 255))
               (zi1 (logand (1+ zi) 255))
               (wi1 (logand (1+ wi) 255))
               (xf-1 (1- xf))
               (yf-1 (1- yf))
               (zf-1 (1- zf))
               (wf-1 (1- wf))
               (fs (interpolate/quintic xf))
               (ft (interpolate/quintic yf))
               (fr (interpolate/quintic zf))
               (fq (interpolate/quintic wf)))
      (float
       (u:lerp
        fs
        (u:lerp
         ft
         (u:lerp
          fr
          (u:lerp fq
                  (grad (lookup table xi yi zi wi) xf yf zf wf)
                  (grad (lookup table xi yi zi wi1) xf yf zf wf-1))
          (u:lerp fq
                  (grad (lookup table xi yi zi1 wi) xf yf zf-1 wf)
                  (grad (lookup table xi yi zi1 wi1) xf yf zf-1 wf-1)))
         (u:lerp
          fr
          (u:lerp fq
                  (grad (lookup table xi yi1 zi wi) xf yf-1 zf wf)
                  (grad (lookup table xi yi1 zi wi1) xf yf-1 zf wf-1))
          (u:lerp fq
                  (grad (lookup table xi yi1 zi1 wi) xf yf-1 zf-1 wf)
                  (grad (lookup table xi yi1 zi1 wi1) xf yf-1 zf-1 wf-1))))
        (u:lerp
         ft
         (u:lerp
          fr
          (u:lerp fq
                  (grad (lookup table xi1 yi zi wi) xf-1 yf zf wf)
                  (grad (lookup table xi1 yi zi wi1) xf-1 yf zf wf-1))
          (u:lerp fq
                  (grad (lookup table xi1 yi zi1 wi) xf-1 yf zf-1 wf)
                  (grad (lookup table xi1 yi zi1 wi1) xf-1 yf zf-1 wf-1)))
         (u:lerp
          fr
          (u:lerp fq
                  (grad (lookup table xi1 yi1 zi wi) xf-1 yf-1 zf wf)
                  (grad (lookup table xi1 yi1 zi wi1) xf-1 yf-1 zf wf-1))
          (u:lerp fq
                  (grad (lookup table xi1 yi1 zi1 wi) xf-1 yf-1 zf-1 wf)
                  (grad (lookup table xi1 yi1 zi1 wi1) xf-1 yf-1 zf-1 wf-1)))))
       1f0))))
