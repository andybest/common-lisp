((gl-back-color
  :name "gl_BackColor"
  :stage vertex
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-color
  :name "gl_BackColor"
  :stage tessellation-control
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-color
  :name "gl_BackColor"
  :stage tessellation-control
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-color
  :name "gl_BackColor"
  :stage tessellation-evaluation
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-color
  :name "gl_BackColor"
  :stage tessellation-evaluation
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-color
  :name "gl_BackColor"
  :stage geometry
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-color
  :name "gl_BackColor"
  :stage geometry
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-secondary-color
  :name "gl_BackSecondaryColor"
  :stage vertex
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-secondary-color
  :name "gl_BackSecondaryColor"
  :stage tessellation-control
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-secondary-color
  :name "gl_BackSecondaryColor"
  :stage tessellation-control
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-secondary-color
  :name "gl_BackSecondaryColor"
  :stage tessellation-evaluation
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-secondary-color
  :name "gl_BackSecondaryColor"
  :stage tessellation-evaluation
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-secondary-color
  :name "gl_BackSecondaryColor"
  :stage geometry
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-back-secondary-color
  :name "gl_BackSecondaryColor"
  :stage geometry
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-base-instance
  :name "gl_BaseInstance"
  :stage vertex
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:460)
  :vulkan t)
 (gl-base-vertex
  :name "gl_BaseVertex"
  :stage vertex
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:460)
  :vulkan t)
 (gl-clip-distance
  :name "gl_ClipDistance"
  :stage vertex
  :qualifier out
  :type (float)
  :block "gl_PerVertex"
  :instance nil
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-clip-distance
  :name "gl_ClipDistance"
  :stage tessellation-control
  :qualifier in
  :type (float)
  :block "gl_PerVertex"
  :instance ((gl-in gl-max-patch-vertices) "gl_in")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-clip-distance
  :name "gl_ClipDistance"
  :stage tessellation-control
  :qualifier out
  :type (float)
  :block "gl_PerVertex"
  :instance ((gl-out) "gl_out")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-clip-distance
  :name "gl_ClipDistance"
  :stage tessellation-evaluation
  :qualifier in
  :type (float)
  :block "gl_PerVertex"
  :instance ((gl-in gl-max-patch-vertices) "gl_in")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-clip-distance
  :name "gl_ClipDistance"
  :stage tessellation-evaluation
  :qualifier out
  :type (float)
  :block "gl_PerVertex"
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-clip-distance
  :name "gl_ClipDistance"
  :stage geometry
  :qualifier in
  :type (float)
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-clip-distance
  :name "gl_ClipDistance"
  :stage geometry
  :qualifier out
  :type (float)
  :block "gl_PerVertex"
  :instance nil
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-clip-distance
  :name "gl_ClipDistance"
  :stage fragment
  :qualifier in
  :type (float)
  :block nil
  :instance nil
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-clip-vertex
  :name "gl_ClipVertex"
  :stage vertex
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:110 :120 :130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-clip-vertex
  :name "gl_ClipVertex"
  :stage tessellation-control
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-clip-vertex
  :name "gl_ClipVertex"
  :stage tessellation-control
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-clip-vertex
  :name "gl_ClipVertex"
  :stage tessellation-evaluation
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-clip-vertex
  :name "gl_ClipVertex"
  :stage tessellation-evaluation
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-clip-vertex
  :name "gl_ClipVertex"
  :stage geometry
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-clip-vertex
  :name "gl_ClipVertex"
  :stage geometry
  :qualifier out
  :type vec4
  :block nil
  :instance nil
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-color
  :name "gl_Color"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-color
  :name "gl_Color"
  :stage fragment
  :qualifier in
  :type vec4
  :block "gl_PerFragment"
  :instance nil
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-cull-distance
  :name "gl_CullDistance"
  :stage vertex
  :qualifier out
  :type (float)
  :block "gl_PerVertex"
  :instance nil
  :versions (:450 :460)
  :vulkan t)
 (gl-cull-distance
  :name "gl_CullDistance"
  :stage tessellation-control
  :qualifier in
  :type (float)
  :block "gl_PerVertex"
  :instance ((gl-in gl-max-patch-vertices) "gl_in")
  :versions (:450 :460)
  :vulkan t)
 (gl-cull-distance
  :name "gl_CullDistance"
  :stage tessellation-control
  :qualifier out
  :type (float)
  :block "gl_PerVertex"
  :instance ((gl-out) "gl_out")
  :versions (:450 :460)
  :vulkan t)
 (gl-cull-distance
  :name "gl_CullDistance"
  :stage tessellation-evaluation
  :qualifier in
  :type (float)
  :block "gl_PerVertex"
  :instance ((gl-in gl-max-patch-vertices) "gl_in")
  :versions (:450 :460)
  :vulkan t)
 (gl-cull-distance
  :name "gl_CullDistance"
  :stage tessellation-evaluation
  :qualifier out
  :type (float)
  :block "gl_PerVertex"
  :instance nil
  :versions (:450 :460)
  :vulkan t)
 (gl-cull-distance
  :name "gl_CullDistance"
  :stage geometry
  :qualifier in
  :type (float)
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:450 :460)
  :vulkan t)
 (gl-cull-distance
  :name "gl_CullDistance"
  :stage geometry
  :qualifier out
  :type (float)
  :block "gl_PerVertex"
  :instance nil
  :versions (:450 :460)
  :vulkan t)
 (gl-cull-distance
  :name "gl_CullDistance"
  :stage fragment
  :qualifier in
  :type (float)
  :block nil
  :instance nil
  :versions (:450 :460)
  :vulkan t)
 (gl-draw-id
  :name "gl_DrawID"
  :stage vertex
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:460)
  :vulkan t)
 (gl-fog-coord
  :name "gl_FogCoord"
  :stage vertex
  :qualifier in
  :type float
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-fog-frag-coord
  :name "gl_FogFragCoord"
  :stage vertex
  :qualifier out
  :type float
  :block "gl_PerVertex"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-fog-frag-coord
  :name "gl_FogFragCoord"
  :stage tessellation-control
  :qualifier in
  :type float
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-fog-frag-coord
  :name "gl_FogFragCoord"
  :stage tessellation-control
  :qualifier out
  :type float
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-fog-frag-coord
  :name "gl_FogFragCoord"
  :stage tessellation-evaluation
  :qualifier in
  :type float
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-fog-frag-coord
  :name "gl_FogFragCoord"
  :stage tessellation-evaluation
  :qualifier out
  :type float
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-fog-frag-coord
  :name "gl_FogFragCoord"
  :stage geometry
  :qualifier in
  :type float
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-fog-frag-coord
  :name "gl_FogFragCoord"
  :stage geometry
  :qualifier out
  :type float
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-fog-frag-coord
  :name "gl_FogFragCoord"
  :stage fragment
  :qualifier in
  :type float
  :block "gl_PerFragment"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-frag-color
  :name "gl_FragColor"
  :stage fragment
  :qualifier out
  :type vec4
  :block nil
  :instance nil
  :versions (:110 :120 :130 :140 :150 :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-frag-coord
  :name "gl_FragCoord"
  :stage fragment
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-frag-data
  :name "gl_FragData"
  :stage fragment
  :qualifier out
  :type (vec4 gl-max-draw-buffers)
  :block nil
  :instance nil
  :versions (:110 :120 :130 :140 :150 :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-frag-depth
  :name "gl_FragDepth"
  :stage fragment
  :qualifier out
  :type float
  :block nil
  :instance nil
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-front-color
  :name "gl_FrontColor"
  :stage vertex
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-color
  :name "gl_FrontColor"
  :stage tessellation-control
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-color
  :name "gl_FrontColor"
  :stage tessellation-control
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-color
  :name "gl_FrontColor"
  :stage tessellation-evaluation
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-color
  :name "gl_FrontColor"
  :stage tessellation-evaluation
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-color
  :name "gl_FrontColor"
  :stage geometry
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-color
  :name "gl_FrontColor"
  :stage geometry
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-facing
  :name "gl_FrontFacing"
  :stage fragment
  :qualifier in
  :type bool
  :block nil
  :instance nil
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-front-secondary-color
  :name "gl_FrontSecondaryColor"
  :stage vertex
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-secondary-color
  :name "gl_FrontSecondaryColor"
  :stage tessellation-control
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-secondary-color
  :name "gl_FrontSecondaryColor"
  :stage tessellation-control
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-secondary-color
  :name "gl_FrontSecondaryColor"
  :stage tessellation-evaluation
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-secondary-color
  :name "gl_FrontSecondaryColor"
  :stage tessellation-evaluation
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-secondary-color
  :name "gl_FrontSecondaryColor"
  :stage geometry
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-front-secondary-color
  :name "gl_FrontSecondaryColor"
  :stage geometry
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-global-invocation-id
  :name "gl_GlobalInvocationID"
  :stage compute
  :qualifier in
  :type uvec3
  :block nil
  :instance nil
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-helper-invocation
  :name "gl_HelperInvocation"
  :stage fragment
  :qualifier in
  :type bool
  :block nil
  :instance nil
  :versions (:450 :460)
  :vulkan t)
 (gl-instance-id
  :name "gl_InstanceID"
  :stage vertex
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan nil)
 (gl-instance-index
  :name "gl_InstanceIndex"
  :stage vertex
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:460)
  :vulkan only)
 (gl-invocation-id
  :name "gl_InvocationID"
  :stage tessellation-control
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-invocation-id
  :name "gl_InvocationID"
  :stage geometry
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-layer
  :name "gl_Layer"
  :stage geometry
  :qualifier out
  :type int
  :block nil
  :instance nil
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-layer
  :name "gl_Layer"
  :stage fragment
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-local-invocation-id
  :name "gl_LocalInvocationID"
  :stage compute
  :qualifier in
  :type uvec3
  :block nil
  :instance nil
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-local-invocation-index
  :name "gl_LocalInvocationIndex"
  :stage compute
  :qualifier in
  :type uint
  :block nil
  :instance nil
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-multi-tex-coord-0
  :name "gl_MultiTexCoord0"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-multi-tex-coord-1
  :name "gl_MultiTexCoord1"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-multi-tex-coord-2
  :name "gl_MultiTexCoord2"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-multi-tex-coord-3
  :name "gl_MultiTexCoord3"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-multi-tex-coord-4
  :name "gl_MultiTexCoord4"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-multi-tex-coord-5
  :name "gl_MultiTexCoord5"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-multi-tex-coord-6
  :name "gl_MultiTexCoord6"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-multi-tex-coord-7
  :name "gl_MultiTexCoord7"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-normal
  :name "gl_Normal"
  :stage vertex
  :qualifier in
  :type vec3
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-num-work-groups
  :name "gl_NumWorkGroups"
  :stage compute
  :qualifier in
  :type uvec3
  :block nil
  :instance nil
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-patch-vertices-in
  :name "gl_PatchVerticesIn"
  :stage tessellation-control
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-patch-vertices-in
  :name "gl_PatchVerticesIn"
  :stage tessellation-evaluation
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-point-coord
  :name "gl_PointCoord"
  :stage fragment
  :qualifier in
  :type vec2
  :block nil
  :instance nil
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-point-size
  :name "gl_PointSize"
  :stage vertex
  :qualifier out
  :type float
  :block "gl_PerVertex"
  :instance nil
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-point-size
  :name "gl_PointSize"
  :stage tessellation-control
  :qualifier in
  :type float
  :block "gl_PerVertex"
  :instance ((gl-in gl-max-patch-vertices) "gl_in")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-point-size
  :name "gl_PointSize"
  :stage tessellation-control
  :qualifier out
  :type float
  :block "gl_PerVertex"
  :instance ((gl-out) "gl_out")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-point-size
  :name "gl_PointSize"
  :stage tessellation-evaluation
  :qualifier in
  :type float
  :block "gl_PerVertex"
  :instance ((gl-in gl-max-patch-vertices) "gl_in")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-point-size
  :name "gl_PointSize"
  :stage tessellation-evaluation
  :qualifier out
  :type float
  :block "gl_PerVertex"
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-point-size
  :name "gl_PointSize"
  :stage geometry
  :qualifier in
  :type float
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-point-size
  :name "gl_PointSize"
  :stage geometry
  :qualifier out
  :type float
  :block "gl_PerVertex"
  :instance nil
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-position
  :name "gl_Position"
  :stage vertex
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-position
  :name "gl_Position"
  :stage tessellation-control
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in gl-max-patch-vertices) "gl_in")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-position
  :name "gl_Position"
  :stage tessellation-control
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-out) "gl_out")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-position
  :name "gl_Position"
  :stage tessellation-evaluation
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in gl-max-patch-vertices) "gl_in")
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-position
  :name "gl_Position"
  :stage tessellation-evaluation
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-position
  :name "gl_Position"
  :stage geometry
  :qualifier in
  :type vec4
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-position
  :name "gl_Position"
  :stage geometry
  :qualifier out
  :type vec4
  :block "gl_PerVertex"
  :instance nil
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-primitive-id
  :name "gl_PrimitiveID"
  :stage tessellation-control
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-primitive-id
  :name "gl_PrimitiveID"
  :stage tessellation-evaluation
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-primitive-id
  :name "gl_PrimitiveID"
  :stage geometry
  :qualifier out
  :type int
  :block nil
  :instance nil
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-primitive-id
  :name "gl_PrimitiveID"
  :stage fragment
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-primitive-id-in
  :name "gl_PrimitiveIDIn"
  :stage geometry
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-sample-id
  :name "gl_SampleID"
  :stage fragment
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-sample-mask
  :name "gl_SampleMask"
  :stage fragment
  :qualifier out
  :type (int)
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-sample-mask-in
  :name "gl_SampleMaskIn"
  :stage fragment
  :qualifier in
  :type (int)
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-sample-position
  :name "gl_SamplePosition"
  :stage fragment
  :qualifier in
  :type vec2
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-secondary-color
  :name "gl_SecondaryColor"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-secondary-color
  :name "gl_SecondaryColor"
  :stage fragment
  :qualifier in
  :type vec4
  :block "gl_PerFragment"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tess-level-inner
  :name "gl_TessLevelInner"
  :stage tessellation-control
  :qualifier out
  :type (float 2)
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-tess-level-inner
  :name "gl_TessLevelInner"
  :stage tessellation-evaluation
  :qualifier in
  :type (float 2)
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-tess-level-outer
  :name "gl_TessLevelOuter"
  :stage tessellation-control
  :qualifier out
  :type (float 4)
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-tess-level-outer
  :name "gl_TessLevelOuter"
  :stage tessellation-evaluation
  :qualifier in
  :type (float 4)
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-tex-coord
  :name "gl_TexCoord"
  :stage vertex
  :qualifier out
  :type (vec4)
  :block "gl_PerVertex"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tex-coord
  :name "gl_TexCoord"
  :stage tessellation-control
  :qualifier in
  :type (vec4)
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tex-coord
  :name "gl_TexCoord"
  :stage tessellation-control
  :qualifier out
  :type (vec4)
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tex-coord
  :name "gl_TexCoord"
  :stage tessellation-evaluation
  :qualifier in
  :type (vec4)
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tex-coord
  :name "gl_TexCoord"
  :stage tessellation-evaluation
  :qualifier out
  :type (vec4)
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tex-coord
  :name "gl_TexCoord"
  :stage geometry
  :qualifier in
  :type (vec4)
  :block "gl_PerVertex"
  :instance ((gl-in) "gl_in")
  :versions (:150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tex-coord
  :name "gl_TexCoord"
  :stage geometry
  :qualifier out
  :type (vec4)
  :block "gl_PerVertex"
  :instance nil
  :versions (:400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tex-coord
  :name "gl_TexCoord"
  :stage fragment
  :qualifier in
  :type (vec4)
  :block "gl_PerFragment"
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-tess-coord
  :name "gl_TessCoord"
  :stage tessellation-evaluation
  :qualifier in
  :type vec3
  :block nil
  :instance nil
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-vertex
  :name "gl_Vertex"
  :stage vertex
  :qualifier in
  :type vec4
  :block nil
  :instance nil
  :versions (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-vertex-id
  :name "gl_VertexID"
  :stage vertex
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan nil)
 (gl-vertex-index
  :name "gl_VertexIndex"
  :stage vertex
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:460)
  :vulkan only)
 (gl-viewport-index
  :name "gl_ViewportIndex"
  :stage geometry
  :qualifier out
  :type int
  :block nil
  :instance nil
  :versions (:330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-viewport-index
  :name "gl_ViewportIndex"
  :stage fragment
  :qualifier in
  :type int
  :block nil
  :instance nil
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-work-group-id
  :name "gl_WorkGroupID"
  :stage compute
  :qualifier in
  :type uvec3
  :block nil
  :instance nil
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-work-group-size
  :name "gl_WorkGroupSize"
  :stage compute
  :qualifier const
  :type uvec3
  :block nil
  :instance nil
  :versions (:430 :440 :450 :460)
  :vulkan t))
