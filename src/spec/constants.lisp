((gl-min-program-texel-offset
  :name "gl_MinProgramTexelOffset"
  :qualifier const
  :type int
  :min -8
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-atomic-counter-bindings
  :name "gl_MaxAtomicCounterBindings"
  :qualifier const
  :type int
  :min 1
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-atomic-counter-buffer-size
  :name "gl_MaxAtomicCounterBufferSize"
  :qualifier const
  :type int
  :min ((:420) 16384 (:430 :440 :450 :460) 32)
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-clip-distances
  :name "gl_MaxClipDistances"
  :qualifier const
  :type int
  :min 8
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-clip-planes
  :name "gl_MaxClipPlanes"
  :qualifier const
  :type int
  :min ((:110 :120) 6 (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat) 8)
  :versions (:110 :120 :130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-max-combined-atomic-counter-buffers
  :name "gl_MaxCombinedAtomicCounterBuffers"
  :qualifier const
  :type int
  :min 1
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-combined-atomic-counters
  :name "gl_MaxCombinedAtomicCounters"
  :qualifier const
  :type int
  :min 8
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-combined-clip-and-cull-distances
  :name "gl_MaxCombinedClipAndCullDistances"
  :qualifier const
  :type int
  :min 8
  :versions (:450 :460)
  :vulkan t)
 (gl-max-combined-image-uniforms
  :name "gl_MaxCombinedImageUniforms"
  :qualifier const
  :type int
  :min ((:420) 0 (:430 :440 :450 :460) 8)
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-combined-image-units-and-fragment-outputs
  :name "gl_MaxCombinedImageUnitsAndFragmentOutputs"
  :qualifier const
  :type int
  :min 8
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-combined-shader-output-resources
  :name "gl_MaxCombinedShaderOutputResources"
  :qualifier const
  :type int
  :min 8
  :versions (:440 :450 :460)
  :vulkan t)
 (gl-max-combined-texture-image-units
  :name "gl_MaxCombinedTextureImageUnits"
  :qualifier const
  :type int
  :min ((:110 :120) 2 (:130 :140) 16 (:150 :330) 48 (:400 :410 :420 :430) 80 (:440 :450 :460) 96)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-compute-atomic-counter-buffers
  :name "gl_MaxComputeAtomicCounterBuffers"
  :qualifier const
  :type int
  :min ((:430) 1 (:440 :450 :460) 8)
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-max-compute-atomic-counters
  :name "gl_MaxComputeAtomicCounters"
  :qualifier const
  :type int
  :min 8
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-max-compute-image-uniforms
  :name "gl_MaxComputeImageUniforms"
  :qualifier const
  :type int
  :min 8
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-max-compute-uniform-components
  :name "gl_MaxComputeUniformComponents"
  :qualifier const
  :type int
  :min ((:450) 512 (:430 :440 :460) 1024)
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-max-compute-texture-image-units
  :name "gl_MaxComputeTextureImageUnits"
  :qualifier const
  :type int
  :min 16
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-max-compute-work-group-count
  :name "gl_MaxComputeWorkGroupCount"
  :qualifier const
  :type ivec3
  :min #(65535 65535 65535)
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-max-compute-work-group-size
  :name "gl_MaxComputeWorkGroupSize"
  :qualifier const
  :type ivec3
  :min #(1024 1024 64)
  :versions (:430 :440 :450 :460)
  :vulkan t)
 (gl-max-cull-distances
  :name "gl_MaxCullDistances"
  :qualifier const
  :type int
  :min 8
  :versions (:450 :460)
  :vulkan t)
 (gl-max-draw-buffers
  :name "gl_MaxDrawBuffers"
  :qualifier const
  :type int
  :min ((:110 :120) 1 (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460) 8)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-fragment-atomic-counter-buffers
  :name "gl_MaxFragmentAtomicCounterBuffers"
  :qualifier const
  :type int
  :min 1
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-fragment-atomic-counters
  :name "gl_MaxFragmentAtomicCounters"
  :qualifier const
  :type int
  :min 8
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-fragment-image-uniforms
  :name "gl_MaxFragmentImageUniforms"
  :qualifier const
  :type int
  :min ((:420) 0 (:430 :440 :450 :460) 8)
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-fragment-input-components
  :name "gl_MaxFragmentInputComponents"
  :qualifier const
  :type int
  :min 128
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-fragment-uniform-components
  :name "gl_MaxFragmentUniformComponents"
  :qualifier const
  :type int
  :min ((:110 :120) 64 (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460) 1024)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-fragment-uniform-vectors
  :name "gl_MaxFragmentUniformVectors"
  :qualifier const
  :type int
  :min 256
  :versions (:410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-atomic-counter-buffers
  :name "gl_MaxGeometryAtomicCounterBuffers"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-atomic-counters
  :name "gl_MaxGeometryAtomicCounters"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-image-uniforms
  :name "gl_MaxGeometryImageUniforms"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-input-components
  :name "gl_MaxGeometryInputComponents"
  :qualifier const
  :type int
  :min 64
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-output-components
  :name "gl_MaxGeometryOutputComponents"
  :qualifier const
  :type int
  :min 128
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-output-vertices
  :name "gl_MaxGeometryOutputVertices"
  :qualifier const
  :type int
  :min 256
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-texture-image-units
  :name "gl_MaxGeometryTextureImageUnits"
  :qualifier const
  :type int
  :min 16
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-total-output-components
  :name "gl_MaxGeometryTotalOutputComponents"
  :qualifier const
  :type int
  :min 1024
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-uniform-components
  :name "gl_MaxGeometryUniformComponents"
  :qualifier const
  :type int
  :min 1024
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-geometry-varying-components
  :name "gl_MaxGeometryVaryingComponents"
  :qualifier const
  :type int
  :min 64
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-image-samples
  :name "gl_MaxImageSamples"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-image-units
  :name "gl_MaxImageUnits"
  :qualifier const
  :type int
  :min 8
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-input-attachments
  :name "gl_MaxInputAttachments"
  :qualifier (const highp)
  :type int
  :min 1
  :versions (:460)
  :vulkan only)
 (gl-max-lights
  :name "gl_MaxLights"
  :qualifier const
  :type int
  :min 8
  :versions (:110 :120)
  :vulkan nil)
 (gl-max-patch-vertices
  :name "gl_MaxPatchVertices"
  :qualifier const
  :type int
  :min 32
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-program-texel-offset
  :name "gl_MaxProgramTexelOffset"
  :qualifier const
  :type int
  :min 7
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-samples
  :name "gl_MaxSamples"
  :qualifier const
  :type int
  :min 4
  :versions (:450 :460)
  :vulkan t)
 (gl-max-tess-control-atomic-counter-buffers
  :name "gl_MaxTessControlAtomicCounterBuffers"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-control-atomic-counters
  :name "gl_MaxTessControlAtomicCounters"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-control-image-uniforms
  :name "gl_MaxTessControlImageUniforms"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-control-input-components
  :name "gl_MaxTessControlInputComponents"
  :qualifier const
  :type int
  :min 128
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-control-output-components
  :name "gl_MaxTessControlOutputComponents"
  :qualifier const
  :type int
  :min 128
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-control-texture-image-units
  :name "gl_MaxTessControlTextureImageUnits"
  :qualifier const
  :type int
  :min 16
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-control-total-output-components
  :name "gl_MaxTessControlTotalOutputComponents"
  :qualifier const
  :type int
  :min 4096
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-control-uniform-components
  :name "gl_MaxTessControlUniformComponents"
  :qualifier const
  :type int
  :min 1024
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-evaluation-atomic-counter-buffers
  :name "gl_MaxTessEvaluationAtomicCounterBuffers"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-evaluation-atomic-counters
  :name "gl_MaxTessEvaluationAtomicCounters"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-evaluation-image-uniforms
  :name "gl_MaxTessEvaluationImageUniforms"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-evaluation-input-components
  :name "gl_MaxTessEvaluationInputComponents"
  :qualifier const
  :type int
  :min 128
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-evaluation-output-components
  :name "gl_MaxTessEvaluationOutputComponents"
  :qualifier const
  :type int
  :min 128
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-evaluation-texture-image-units
  :name "gl_MaxTessEvaluationTextureImageUnits"
  :qualifier const
  :type int
  :min 16
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-evaluation-uniform-components
  :name "gl_MaxTessEvaluationUniformComponents"
  :qualifier const
  :type int
  :min 1024
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-gen-level
  :name "gl_MaxTessGenLevel"
  :qualifier const
  :type int
  :min 64
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-tess-patch-components
  :name "gl_MaxTessPatchComponents"
  :qualifier const
  :type int
  :min 120
  :versions (:400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-texture-coords
  :name "gl_MaxTextureCoords"
  :qualifier const
  :type int
  :min ((:110 :120) 2 (:130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat) 8)
  :versions (:110 :120 :130 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-max-texture-image-units
  :name "gl_MaxTextureImageUnits"
  :qualifier const
  :type int
  :min ((:110 :120) 2 (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460) 16)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-texture-units
  :name "gl_MaxTextureUnits"
  :qualifier const
  :type int
  :min ((:110 :120) 2 (:130 :140 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat) 16)
  :versions (:110 :120 :130 :140 :150-compat :330-compat :400-compat :410-compat :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-max-transform-feedback-buffers
  :name "gl_MaxTransformFeedbackBuffers"
  :qualifier const
  :type int
  :min 4
  :versions (:440 :450 :460)
  :vulkan t)
 (gl-max-transform-feedback-interleaved-components
  :name "gl_MaxTransformFeedbackInterleavedComponents"
  :qualifier const
  :type int
  :min 64
  :versions (:440 :450 :460)
  :vulkan t)
 (gl-max-varying-components
  :name "gl_MaxVaryingComponents"
  :qualifier const
  :type int
  :min ((:130 :140) 64 (:150 :330 :400 :410 :420 :430 :440 :450 :460) 60)
  :versions (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-varying-floats
  :name "gl_MaxVaryingFloats"
  :qualifier const
  :type int
  :min ((:110 :120) 32 (:130 :140) 64 (:150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat) 60)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420-compat :430-compat :440-compat :450-compat :460-compat)
  :vulkan nil)
 (gl-max-varying-vectors
  :name "gl_MaxVaryingVectors"
  :qualifier const
  :type int
  :min 15
  :versions (:410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-vertex-atomic-counter-buffers
  :name "gl_MaxVertexAtomicCounterBuffers"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-vertex-atomic-counters
  :name "gl_MaxVertexAtomicCounters"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-vertex-attribs
  :name "gl_MaxVertexAttribs"
  :qualifier const
  :type int
  :min 16
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-vertex-image-uniforms
  :name "gl_MaxVertexImageUniforms"
  :qualifier const
  :type int
  :min 0
  :versions (:420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-vertex-output-components
  :name "gl_MaxVertexOutputComponents"
  :qualifier const
  :type int
  :min 64
  :versions (:150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-vertex-texture-image-units
  :name "gl_MaxVertexTextureImageUnits"
  :qualifier const
  :type int
  :min ((:110 :120) 0 (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460) 16)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-vertex-uniform-components
  :name "gl_MaxVertexUniformComponents"
  :qualifier const
  :type int
  :min ((:110 :120) 512 (:130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460) 1024)
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-vertex-uniform-vectors
  :name "gl_MaxVertexUniformVectors"
  :qualifier const
  :type int
  :min 256
  :versions (:410 :420 :430 :440 :450 :460)
  :vulkan t)
 (gl-max-viewports
  :name "gl_MaxViewports"
  :qualifier const
  :type int
  :min 16
  :versions (:410 :420 :430 :440 :450 :460)
  :vulkan t))
