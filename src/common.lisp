(in-package #:%coherent-noise.internal)

(deftype f50 () '(double-float #.(- (expt 2d0 50)) #.(expt 2d0 50)))

(u:define-constant +perlin-permutation+
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

(u:define-constant +prime-x+ 501125321)

(u:define-constant +prime-y+ 1136930381)

(u:define-constant +prime-z+ 1720413743)

(defstruct (sampler
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (rng nil :type (or rng:generator null)))

(u:define-printer (sampler stream :type nil :identity t)
  (format stream "~a" (class-name (class-of sampler))))

(defmacro lookup (table &body (first . rest))
  (if rest
      `(aref ,table (logand (+ ,first (lookup ,table ,@rest)) 255))
      `(aref ,table (logand ,first 255))))

(declaim (inline interpolate-cubic))
(defun interpolate-cubic (a b c d alpha)
  (declare (optimize speed)
           (u:f32 a b c d alpha))
  (let* ((x (- (- d c) (- a b)))
         (y (- (- a b) x))
         (z (- c a)))
    (+ (* x alpha alpha alpha) (* y alpha alpha) (* z alpha) a)))

(declaim (inline cubic-curve))
(defun cubic-curve (x)
  (* x x (- 3.0 (* x 2.0))))

(declaim (inline quintic-curve))
(defun quintic-curve (x)
  (* x x x (+ (* x (- (* x 6.0) 15.0)) 10.0)))

(defun make-rng (seed)
  (unless (or (stringp seed)
              (null seed))
    (error 'invalid-seed :seed seed))
  (let ((rng (rng:make-generator seed)))
    (values rng (nth-value 1 (rng:get-seed rng)))))

(defun make-fractal-sources (generator rng count)
  (let ((sources (make-array count)))
    (dotimes (i count)
      (let ((rng (rng:make-generator rng)))
        (setf (aref sources i) (funcall generator :seed (rng:get-seed rng)))))
    sources))

(defgeneric sample (sampler x &optional y z w))
