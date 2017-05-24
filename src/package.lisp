(defpackage #:gamebox-math
  (:use #:cl
        #:alexandria)
  ;; vectors
  (:export #:vec #:+zero-vector+ #:with-vector #:with-vectors #:vref
           #:vec-copy! #:vcp! #:vec-copy #:vcp
           #:vec-clamp! #:vclamp! #:vec-clamp #:vclamp
           #:vec-stabilize! #:vstab! #:vec-stabilize #:vstab
           #:vec-zero! #:vzero! #:vec-zero #:vzero
           #:vec-to-list #:v->list #:vec-from-list #:list->v
           #:vec= #:v=
           #:vec~ #:v~
           #:vec+! #:v+! #:vec+ #:v+
           #:vec-! #:v-! #:vec- #:v-
           #:vec-hadamard*! #:vhad*! #:vec-hadamard* #:vhad*
           #:vec-hadamard/! #:vhad/! #:vec-hadamard/ #:vhad/
           #:vec-scale! #:vscale! #:vec-scale #:vscale
           #:vec-dot #:vdot
           #:vec-magnitude-squared #:vmagsq
           #:vec-magnitude #:vmag
           #:vec-normalize! #:vnormalize! #:vec-normalize #:vnormalize
           #:vec-round! #:vround! #:vec-round #:vround
           #:vec-abs! #:vabs! #:vec-abs #:vabs
           #:vec-negate! #:vneg! #:vec-negate #:vneg
           #:vec-cross! #:vcross! #:vec-cross #:vcross
           #:vec-box #:vbox
           #:vec-angle #:vangle
           #:vec-zero-p #:vzerop
           #:vec-direction= #:vdir=
           #:vec-parallel-p #:vparallelp
           #:vec-lerp! #:vlerp! #:vec-lerp #:vlerp
           #:vec-min! #:vmin! #:vec-min #:vmin
           #:vec-max! #:vmax! #:vec-max #:vmax)
  ;; matrices
  (:export #:matrix #:+identity-matrix+ #:with-matrix #:with-matrices #:mref
           #:matrix-identity! #:mid! #:matrix-identity #:mid
           #:matrix-copy! #:mcp! #:matrix-copy #:mcp
           #:matrix-clamp! #:mclamp! #:matrix-clamp #:mclamp
           #:matrix*! #:m*! #:matrix* #:m*
           #:matrix-translation-to-vec! #:mtr->v! #:matrix-translation-to-vec #:mtr->v
           #:matrix-translation-from-vec! #:v->mtr! #:matrix-translation-from-vec #:v->mtr
           #:matrix-rotate-local! #:mrotl! #:matrix-rotate-local #:mrotl
           #:matrix-rotate! #:mrot! #:matrix-rotate #:mrot
           #:matrix-copy-rotation! #:mcprot! #:matrix-copy-rotation #:mcprot
           #:matrix-rotation-to-vec! #:mrot->v! #:matrix-rotation-to-vec #:mrot->v
           #:matrix-rotation-from-vec! #:v->mrot! #:matrix-rotation-from-vec #:v->mrot
           #:matrix*vec! #:m*v! #:matrix*vec #:m*v
           #:matrix-transpose! #:mtranspose! #:matrix-transpose #:mtranspose
           #:matrix-orthogonal-p #:morthop
           #:matrix-trace #:mtrace
           #:matrix-determinant #:mdet
           #:matrix-invert-orthogonal! #:minvtortho! #:matrix-invert-orthogonal #:minvtortho
           #:matrix-invert! #:minvt! #:matrix-invert #:minvt
           #:make-view-matrix! #:mkview! #:make-view-matrix #:mkview
           #:make-orthographic-matrix! #:mkortho! #:make-orthographic-matrix #:mkortho
           #:make-perspective-matrix! #:mkpersp! #:make-perspective-matrix #:mkpersp)
  ;; quaternions
  (:export #:quat #:with-quat #:with-quats #:qref
           #:quat-identity! #:qid! #:quat-identity #:qid
           #:quat-copy! #:qcp! #:quat-copy #:qcp
           #:quat+! #:q+! #:quat+ #:q+
           #:quat*! #:q*! #:quat* #:q*
           #:quat-scale! #:qscale! #:quat-scale #:qscale
           #:quat-conjugate! #:qconj! #:quat-conjugate #:qconj
           #:quat-magnitude-squared #:qmagsq
           #:quat-magnitude #:qmag
           #:quat-normalize! #:qnormalize! #:quat-normalize #:qnormalize
           #:quat-negate! #:qneg! #:quat-negate #:qneg
           #:quat-dot #:qdot
           #:quat-inverse! #:qinv! #:quat-inverse #:qinv
           #:quat-rotate! #:qrot! #:quat-rotate #:qrot
           #:quat-to-vec! #:q->v! #:quat-to-vec #:q->v
           #:quat-from-vec! #:v->q! #:quat-from-vec #:v->q
           #:quat-to-matrix! #:q->m! #:quat-to-matrix #:q->m
           #:quat-from-matrix! #:m->q! #:quat-from-matrix #:m->q
           #:quat-slerp! #:qslerp! #:quat-slerp #:qslerp)
  ;; math
  (:export #:point-distance #:ptdist
           #:point-translate! #:pttr! #:point-translate #:pttr
           #:point-near-p #:ptnearp
           #:interpolate-transforms! #:mslerp! #:interpolate-transforms #:mslerp
           #:line-direction
           #:line-segment-midpoint
           #:line-plane-intersect
           #:line-point-distance))
