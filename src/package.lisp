(defpackage #:gamebox-math
  (:nicknames #:box.math)
  (:use #:cl
        #:alexandria)

  ;; vec2
  (:export #:vec2 #:v2ref #:+zero-vec2+
           #:with-vec2 #:with-vec2s
           #:vec2-test #:v2test
           #:vec2-copy! #:v2cp! #:vec2-copy #:v2cp
           #:vec2-clamp! #:v2clamp! #:vec2-clamp #:v2clamp
           #:vec2-stabilize! #:v2stab! #:vec2-stabilize #:v2stab
           #:vec2-zero! #:v2zero! #:vec2-zero #:v2zero
           #:vec2-to-list #:v2->list #:vec2-from-list #:list->v2
           #:vec2= #:v2=
           #:vec2~ #:v2~
           #:vec2+! #:v2+! #:vec2+ #:v2+
           #:vec2-! #:v2-! #:vec2- #:v2-
           #:vec2-hadamard*! #:v2had*! #:vec2-hadamard* #:v2had*
           #:vec2-hadamard/! #:v2had/! #:vec2-hadamard/ #:v2had/
           #:vec2-scale! #:v2scale! #:vec2-scale #:v2scale
           #:vec2-dot #:v2dot
           #:vec2-magnitude-squared #:v2magsq
           #:vec2-magnitude #:v2mag
           #:vec2-normalize! #:v2normalize! #:vec2-normalize #:v2normalize
           #:vec2-round! #:v2round! #:vec2-round #:v2round
           #:vec2-abs! #:v2abs! #:vec2-abs #:v2abs
           #:vec2-negate! #:v2neg! #:vec2-negate #:v2neg
           #:vec2-angle #:v2angle
           #:vec2-zero-p #:v2zerop
           #:vec2-direction= #:v2dir=
           #:vec2-lerp! #:v2lerp! #:vec2-lerp #:v2lerp
           #:vec2< #:v2<
           #:vec2<= #:v2<=
           #:vec2> #:v2>
           #:vec2>= #:v2>=
           #:vec2-min! #:v2min! #:vec2-min #:v2min
           #:vec2-max! #:v2max! #:vec2-max #:v2max)

  ;; vec3
  (:export #:vec3 #:v3ref #:+zero-vec3+
           #:with-vec3 #:with-vec3s
           #:vec3-test #:v3test
           #:vec3-copy! #:v3cp! #:vec3-copy #:v3cp
           #:vec3-clamp! #:v3clamp! #:vec3-clamp #:v3clamp
           #:vec3-stabilize! #:v3stab! #:vec3-stabilize #:v3stab
           #:vec3-zero! #:v3zero! #:vec3-zero #:v3zero
           #:vec3-to-list #:v3->list #:vec3-from-list #:list->v3
           #:vec3= #:v3=
           #:vec3~ #:v3~
           #:vec3+! #:v3+! #:vec3+ #:v3+
           #:vec3-! #:v3-! #:vec3- #:v3-
           #:vec3-hadamard*! #:v3had*! #:vec3-hadamard* #:v3had*
           #:vec3-hadamard/! #:v3had/! #:vec3-hadamard/ #:v3had/
           #:vec3-scale! #:v3scale! #:vec3-scale #:v3scale
           #:vec3-dot #:v3dot
           #:vec3-magnitude-squared #:v3magsq
           #:vec3-magnitude #:v3mag
           #:vec3-normalize! #:v3normalize! #:vec3-normalize #:v3normalize
           #:vec3-round! #:v3round! #:vec3-round #:v3round
           #:vec3-abs! #:v3abs! #:vec3-abs #:v3abs
           #:vec3-negate! #:v3neg! #:vec3-negate #:v3neg
           #:vec3-cross! #:v3cross! #:vec3-cross #:v3cross
           #:vec3-box #:v3box
           #:vec3-angle #:v3angle
           #:vec3-zero-p #:v3zerop
           #:vec3-direction= #:v3dir=
           #:vec3-parallel-p #:v3parallelp
           #:vec3-lerp! #:v3lerp! #:vec3-lerp #:v3lerp
           #:vec3< #:v3<
           #:vec3<= #:v3<=
           #:vec3> #:v3>
           #:vec3>= #:v3>=
           #:vec3-min! #:v3min! #:vec3-min #:v3min
           #:vec3-max! #:v3max! #:vec3-max #:v3max)

  ;; vec4
  (:export #:vec4 #:v4ref #:+zero-vec4+
           #:with-vec4 #:with-vec4s
           #:vec4-test #:v4test
           #:vec4-copy! #:v4cp! #:vec4-copy #:v4cp
           #:vec4-clamp! #:v4clamp! #:vec4-clamp #:v4clamp
           #:vec4-stabilize! #:v4stab! #:vec4-stabilize #:v4stab
           #:vec4-zero! #:v4zero! #:vec4-zero #:v4zero
           #:vec4-to-list #:v4->list #:vec4-from-list #:list->v4
           #:vec4= #:v4=
           #:vec4~ #:v4~
           #:vec4+! #:v4+! #:vec4+ #:v4+
           #:vec4-! #:v4-! #:vec4- #:v4-
           #:vec4-hadamard*! #:v4had*! #:vec4-hadamard* #:v4had*
           #:vec4-hadamard/! #:v4had/! #:vec4-hadamard/ #:v4had/
           #:vec4-scale! #:v4scale! #:vec4-scale #:v4scale
           #:vec4-dot #:v4dot
           #:vec4-magnitude-squared #:v4magsq
           #:vec4-magnitude #:v4mag
           #:vec4-normalize! #:v4normalize! #:vec4-normalize #:v4normalize
           #:vec4-round! #:v4round! #:vec4-round #:v4round
           #:vec4-abs! #:v4abs! #:vec4-abs #:v4abs
           #:vec4-negate! #:v4neg! #:vec4-negate #:v4neg
           #:vec4-zero-p #:v4zerop
           #:vec4-lerp! #:v4lerp! #:vec4-lerp #:v4lerp
           #:vec4< #:v4<
           #:vec4<= #:v4<=
           #:vec4> #:v4>
           #:vec4>= #:v4>=
           #:vec4-min! #:v4min! #:vec4-min #:v4min
           #:vec4-max! #:v4max! #:vec4-max #:v4max)

  ;; matrix
  (:export #:matrix #:with-matrix #:with-matrices #:mref
           #:+mid+
           #:zero-matrix! #:mzero! #:zero-matrix #:mzero #:+zero-matrix+
           #:matrix-test #:mtest
           #:matrix-identity! #:mid! #:matrix-identity #:mid
           #:matrix= #:m=
           #:matrix~ #:m~
           #:matrix-copy! #:mcp! #:matrix-copy #:mcp
           #:matrix-clamp! #:mclamp! #:matrix-clamp #:mclamp
           #:matrix*! #:m*! #:matrix* #:m*
           #:matrix-translation-to-vec3! #:mtr->v3! #:matrix-translation-to-vec3 #:mtr->v3
           #:matrix-translation-from-vec3! #:v3->mtr! #:matrix-translation-from-vec3 #:v3->mtr
           #:matrix-translate! #:mtr! #:matrix-translate #:mtr
           #:matrix-copy-rotation! #:mcprot! #:matrix-copy-rotation #:mcprot
           #:matrix-rotation-to-vec3! #:mrot->v3! #:matrix-rotation-to-vec3 #:mrot->v3
           #:matrix-rotation-from-vec3! #:v3->mrot! #:matrix-rotation-from-vec3 #:v3->mrot
           #:matrix-rotate! #:mrot! #:matrix-rotate #:mrot
           #:matrix-scale-to-vec3! #:mscale->v3! #:matrix-scale-to-vec3 #:mscale->v3
           #:matrix-scale-from-vec3! #:v3->mscale! #:matrix-scale-from-vec3 #:v3->mscale
           #:matrix-scale! #:mscale! #:matrix-scale #:mscale
           #:matrix*vec3! #:m*v3! #:matrix*vec3 #:m*v3
           #:matrix*vec4! #:m*v4! #:matrix*vec4 #:m*v4
           #:matrix-transpose! #:mtranspose! #:matrix-transpose #:mtranspose
           #:matrix-orthogonal-p #:morthop
           #:matrix-orthogonalize! #:mortho! #:matrix-orthogonalize #:mortho
           #:matrix-trace #:mtrace
           #:matrix-determinant #:mdet
           #:matrix-invert-orthogonal! #:minvtortho! #:matrix-invert-orthogonal #:minvtortho
           #:matrix-invert! #:minvt! #:matrix-invert #:minvt
           #:make-view-matrix! #:mkview! #:make-view-matrix #:mkview
           #:make-orthographic-matrix! #:mkortho! #:make-orthographic-matrix #:mkortho
           #:make-perspective-matrix! #:mkpersp! #:make-perspective-matrix #:mkpersp)

  ;; quaternion
  (:export #:quat #:with-quat #:with-quats #:qref
           #:+qid+
           #:quat-identity! #:qid! #:quat-identity #:qid
           #:quat-zero! #:qzero! #:quat-zero #:qzero
           #:quat= #:q=
           #:quat~ #:q~
           #:quat-copy! #:qcp! #:quat-copy #:qcp
           #:quat+! #:q+! #:quat+ #:q+
           #:quat-! #:q-! #:quat- #:q-
           #:quat*! #:q*! #:quat* #:q*
           #:quat-scale! #:qscale! #:quat-scale #:qscale
           #:quat-cross! #:qcross! #:quat-cross #:qcross
           #:quat-conjugate! #:qconj! #:quat-conjugate #:qconj
           #:quat-magnitude-squared #:qmagsq
           #:quat-magnitude #:qmag
           #:quat-normalize! #:qnormalize! #:quat-normalize #:qnormalize
           #:quat-negate! #:qneg! #:quat-negate #:qneg
           #:quat-dot #:qdot
           #:quat-inverse! #:qinv! #:quat-inverse #:qinv
           #:quat-rotate! #:qrot! #:quat-rotate #:qrot
           #:quat-to-vec3! #:q->v3! #:quat-to-vec3 #:q->v3
           #:quat-to-vec4! #:q->v4! #:quat-to-vec4 #:q->v4
           #:quat-from-vec3! #:v3->q! #:quat-from-vec3 #:v3->q
           #:quat-from-vec4! #:v4->q! #:quat-from-vec4 #:v4->q
           #:quat-to-matrix! #:q->m! #:quat-to-matrix #:q->m
           #:quat-from-matrix! #:m->q! #:quat-from-matrix #:m->q
           #:quat-slerp! #:qslerp! #:quat-slerp #:qslerp)

  ;; dual quaternion
  (:export #:dquat #:with-dquat #:with-dquats
           #:+dqid+
           #:dquat-identity! #:dqid! #:dquat-identity #:dqid
           #:dquat= #:dq=
           #:dquat~ #:dq~
           #:dquat-copy! #:dqcp! #:dquat-copy #:dqcp
           #:dquat+! #:dq+! #:dquat+ #:dq+
           #:dquat-! #:dq-! #:dquat- #:dq-
           #:dquat*! #:dq*! #:dquat* #:dq*
           #:dquat-scale! #:dqscale! #:dquat-scale #:dqscale
           #:dquat-conjugate! #:dqconj! #:dquat-conjugate #:dqconj
           #:dquat-conjugate-full! #:dqconjf! #:dquat-conjugate-full #:dqconjf
           #:dquat-apply! #:dqapply! #:dqual-apply #:dqapply
           #:dquat-magnitude-squared #:dqmagsq
           #:dquat-magnitude #:dqmag
           #:dquat-normalize! #:dqnormalize! #:dquat-normalize #:dqnormalize
           #:dquat-negate! #:dqneg! #:dquat-negate #:dqneg
           #:dquat-dot #:dqdot
           #:dquat-inverse! #:dqinv! #:dquat-inverse #:dqinv
           #:dquat-translation-to-vec3! #:dqtr->v3! #:dquat-translation-to-vec3 #:dqtr->v3
           #:dquat-translation-from-vec3! #:v3->dqtr! #:dquat-translation-from-vec3 #:v3->dqtr
           #:dquat-translate! #:dqtr! #:dquat-translate #:dqtr
           #:dquat-rotation-to-quat! #:dqrot->q! #:dquat-rotation-to-quat #:dqrot->q
           #:dquat-rotation-from-quat! #:q->dqrot! #:dquat-rotation-from-quat #:q->dqrot
           #:dquat-rotate! #:dqrot! #:dquat-rotate #:dqrot
           #:dquat-to-matrix! #:dq->m! #:dquat-to-matrix #:dq->m
           #:dquat-from-matrix! #:m->dq! #:dquat-from-matrix #:m->dq
           #:dquat-to-screw-parameters #:dq->screw
           #:dquat-from-screw-parameters! #:screw->dq! #:dquat-from-screw-parameters #:screw->dq
           #:dquat-sclerp! #:dqsclerp! #:dquat-sclerp #:dqsclerp
           #:dquat-nlerp! #:dqnlerp! #:dquat-nlerp #:dqnlerp))
