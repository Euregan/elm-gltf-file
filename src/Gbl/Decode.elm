module Gbl.Decode exposing (..)

import Array exposing (Array)
import Bytes
import Json.Decode
import Json.Decode.Pipeline


type alias Matrix4x4 =
    { c1r1 : Float
    , c2r1 : Float
    , c3r1 : Float
    , c4r1 : Float
    , c1r2 : Float
    , c2r2 : Float
    , c3r2 : Float
    , c4r2 : Float
    , c1r3 : Float
    , c2r3 : Float
    , c3r3 : Float
    , c4r3 : Float
    , c1r4 : Float
    , c2r4 : Float
    , c3r4 : Float
    , c4r4 : Float
    }


type alias Quaternion =
    { x : Float, y : Float, z : Float, w : Float }


tripleDecoder : Json.Decode.Decoder ( Float, Float, Float )
tripleDecoder =
    Json.Decode.map3 (\a b c -> ( a, b, c ))
        (Json.Decode.index 0 Json.Decode.float)
        (Json.Decode.index 1 Json.Decode.float)
        (Json.Decode.index 2 Json.Decode.float)


quaternionDecoder : Json.Decode.Decoder Quaternion
quaternionDecoder =
    Json.Decode.map4 Quaternion
        (Json.Decode.index 0 Json.Decode.float)
        (Json.Decode.index 1 Json.Decode.float)
        (Json.Decode.index 2 Json.Decode.float)
        (Json.Decode.index 3 Json.Decode.float)


matrixDecoder : Json.Decode.Decoder Matrix4x4
matrixDecoder =
    Json.Decode.succeed Matrix4x4
        |> Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 2 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 3 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 4 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 5 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 6 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 7 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 8 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 9 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 10 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 11 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 12 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 13 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 14 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 15 Json.Decode.float)


endianness : Bytes.Endianness
endianness =
    Bytes.LE


decoder : Json.Decode.Decoder Gltf
decoder =
    Json.Decode.succeed Gltf
        |> Json.Decode.Pipeline.optional "extensionsUsed" (Json.Decode.array Json.Decode.string) Array.empty
        |> Json.Decode.Pipeline.optional "extensionsRequired" (Json.Decode.array Json.Decode.string) Array.empty
        |> Json.Decode.Pipeline.optional "accessors" (Json.Decode.array accessorDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "animations" (Json.Decode.array animationDecoder) Array.empty
        |> Json.Decode.Pipeline.required "asset" assetDecoder
        |> Json.Decode.Pipeline.optional "buffers" (Json.Decode.array bufferDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "bufferViews" (Json.Decode.array bufferViewDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "cameras" (Json.Decode.array cameraDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "images" (Json.Decode.array imageDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "materials" (Json.Decode.array materialDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "meshes" (Json.Decode.array mesheDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "nodes" (Json.Decode.array nodeDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "samplers" (Json.Decode.array samplerDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "scene" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "scenes" (Json.Decode.array sceneDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "skins" (Json.Decode.array skinDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "textures" (Json.Decode.array textureDecoder) Array.empty
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


accessorDecoder : Json.Decode.Decoder Accessor
accessorDecoder =
    Json.Decode.succeed Accessor
        |> Json.Decode.Pipeline.optional "bufferView" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "byteOffset" Json.Decode.int 0
        |> Json.Decode.Pipeline.required "componentType" accessorComponentTypeDecoder
        |> Json.Decode.Pipeline.optional "normalized" Json.Decode.bool False
        |> Json.Decode.Pipeline.required "count" Json.Decode.int
        |> Json.Decode.Pipeline.required "type_" accessorTypeDecoder
        |> Json.Decode.Pipeline.optional "max" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "min" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "sparse" (Json.Decode.nullable accessorSparseDecoder) Nothing
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


accessorComponentTypeDecoder : Json.Decode.Decoder AccessorComponentType
accessorComponentTypeDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\code ->
                case code of
                    5120 ->
                        Json.Decode.succeed Byte

                    5121 ->
                        Json.Decode.succeed UnsignedByte

                    5122 ->
                        Json.Decode.succeed Short

                    5123 ->
                        Json.Decode.succeed UnsignedShort

                    5125 ->
                        Json.Decode.succeed UnsignedInt

                    5126 ->
                        Json.Decode.succeed Float

                    number ->
                        Json.Decode.fail <| "Component type " ++ String.fromInt number ++ " is unknown"
            )


accessorSparseDecoder : Json.Decode.Decoder AccessorSparse
accessorSparseDecoder =
    Json.Decode.succeed AccessorSparse
        |> Json.Decode.Pipeline.required "count" Json.Decode.int
        |> Json.Decode.Pipeline.required "indices" accessorSparseIndicesDecoder
        |> Json.Decode.Pipeline.required "values" accessorSparseValuesDecoder
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


accessorSparseIndicesDecoder : Json.Decode.Decoder AccessorSparseIndices
accessorSparseIndicesDecoder =
    Json.Decode.succeed AccessorSparseIndices
        |> Json.Decode.Pipeline.required "bufferView" Json.Decode.int
        |> Json.Decode.Pipeline.optional "byteOffset" Json.Decode.int 0
        |> Json.Decode.Pipeline.required "componentType" accessorSparseIndicesComponentTypeDecoder
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


accessorSparseIndicesComponentTypeDecoder : Json.Decode.Decoder AccessorSparseIndicesComponentType
accessorSparseIndicesComponentTypeDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\code ->
                case code of
                    5121 ->
                        Json.Decode.succeed IndicesUnsignedByte

                    5123 ->
                        Json.Decode.succeed IndicesUnsignedShort

                    5125 ->
                        Json.Decode.succeed IndicesUnsignedInt

                    number ->
                        Json.Decode.fail <| "Component type " ++ String.fromInt number ++ " is unknown"
            )


accessorSparseValuesDecoder : Json.Decode.Decoder AccessorSparseValues
accessorSparseValuesDecoder =
    Json.Decode.succeed AccessorSparseValues
        |> Json.Decode.Pipeline.required "bufferView" Json.Decode.int
        |> Json.Decode.Pipeline.optional "byteOffset" Json.Decode.int 0
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


accessorTypeDecoder : Json.Decode.Decoder AccessorType
accessorTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "SCALAR" ->
                        Json.Decode.succeed Scalar

                    "VEC2" ->
                        Json.Decode.succeed Vec2

                    "VEC3" ->
                        Json.Decode.succeed Vec3

                    "VEC4" ->
                        Json.Decode.succeed Vec4

                    "MAT2" ->
                        Json.Decode.succeed Mat2

                    "MAT3" ->
                        Json.Decode.succeed Mat3

                    "MAT4" ->
                        Json.Decode.succeed Mat4

                    t ->
                        Json.Decode.fail <| "Accessor type " ++ t ++ " is unknown"
            )


animationDecoder : Json.Decode.Decoder Animation
animationDecoder =
    Json.Decode.succeed Animation
        |> Json.Decode.Pipeline.required "channels" (Json.Decode.array animationChannelDecoder)
        |> Json.Decode.Pipeline.required "samplers" (Json.Decode.array animationSamplerDecoder)
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


animationChannelDecoder : Json.Decode.Decoder AnimationChannel
animationChannelDecoder =
    Json.Decode.succeed AnimationChannel
        |> Json.Decode.Pipeline.required "sampler" Json.Decode.int
        |> Json.Decode.Pipeline.required "target" animationChannelTargetDecoder
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


animationChannelTargetDecoder : Json.Decode.Decoder AnimationChannelTarget
animationChannelTargetDecoder =
    Json.Decode.succeed AnimationChannelTarget
        |> Json.Decode.Pipeline.optional "channels" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.required "path" animationChannelTargetPathDecoder
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


animationChannelTargetPathDecoder : Json.Decode.Decoder AnimationChannelTargetPath
animationChannelTargetPathDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\path ->
                case path of
                    "translation" ->
                        Json.Decode.succeed Translation

                    "rotation" ->
                        Json.Decode.succeed Rotation

                    "scale" ->
                        Json.Decode.succeed Scale

                    "weights" ->
                        Json.Decode.succeed Weights

                    p ->
                        Json.Decode.fail <| "Animation channel target " ++ p ++ " is unknown"
            )


animationSamplerDecoder : Json.Decode.Decoder AnimationSampler
animationSamplerDecoder =
    Json.Decode.succeed AnimationSampler
        |> Json.Decode.Pipeline.required "input" Json.Decode.int
        |> Json.Decode.Pipeline.optional "interpolation" animationSamplerInterpolationDecoder Linear
        |> Json.Decode.Pipeline.required "output" Json.Decode.int
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


animationSamplerInterpolationDecoder : Json.Decode.Decoder AnimationSamplerInterpolation
animationSamplerInterpolationDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\interpolation ->
                case interpolation of
                    "LINEAR" ->
                        Json.Decode.succeed Linear

                    "STEP" ->
                        Json.Decode.succeed Step

                    "CUBICSPLINE" ->
                        Json.Decode.succeed CubicSpline

                    i ->
                        Json.Decode.fail <| "Animation sampler interpolation " ++ i ++ " is unknown"
            )


assetDecoder : Json.Decode.Decoder Asset
assetDecoder =
    Json.Decode.succeed Asset
        |> Json.Decode.Pipeline.optional "copyright" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "generator" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.required "version" Json.Decode.string
        |> Json.Decode.Pipeline.optional "minVersion" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


bufferDecoder : Json.Decode.Decoder Buffer
bufferDecoder =
    Json.Decode.succeed Buffer
        |> Json.Decode.Pipeline.optional "uri" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.required "byteLength" Json.Decode.int
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


bufferViewDecoder : Json.Decode.Decoder BufferView
bufferViewDecoder =
    Json.Decode.succeed BufferView
        |> Json.Decode.Pipeline.required "buffer" Json.Decode.int
        |> Json.Decode.Pipeline.optional "byteOffset" Json.Decode.int 0
        |> Json.Decode.Pipeline.required "byteLength" Json.Decode.int
        |> Json.Decode.Pipeline.optional "byteStride" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "target" (Json.Decode.nullable bufferViewTargetDecoder) Nothing
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


bufferViewTargetDecoder : Json.Decode.Decoder BufferViewTarget
bufferViewTargetDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\target ->
                case target of
                    34962 ->
                        Json.Decode.succeed ArrayBuffer

                    34963 ->
                        Json.Decode.succeed ElementArrayBuffer

                    t ->
                        Json.Decode.fail <| "Buffer view target " ++ String.fromInt t ++ " is unknown"
            )


cameraDecoder : Json.Decode.Decoder Camera
cameraDecoder =
    Json.Decode.succeed Camera
        |> Json.Decode.Pipeline.optional "orthographic" (Json.Decode.nullable cameraOrthographicDecoder) Nothing
        |> Json.Decode.Pipeline.optional "perspective" (Json.Decode.nullable cameraPerspectiveDecoder) Nothing
        |> Json.Decode.Pipeline.required "type" cameraTypeDecoder
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


cameraOrthographicDecoder : Json.Decode.Decoder CameraOrthographic
cameraOrthographicDecoder =
    Json.Decode.succeed CameraOrthographic
        |> Json.Decode.Pipeline.required "xmag" Json.Decode.float
        |> Json.Decode.Pipeline.required "ymag" Json.Decode.float
        |> Json.Decode.Pipeline.required "zfar" Json.Decode.float
        |> Json.Decode.Pipeline.required "znear" Json.Decode.float
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


cameraPerspectiveDecoder : Json.Decode.Decoder CameraPerspective
cameraPerspectiveDecoder =
    Json.Decode.succeed CameraPerspective
        |> Json.Decode.Pipeline.optional "aspectRatio" (Json.Decode.nullable Json.Decode.float) Nothing
        |> Json.Decode.Pipeline.required "yfov" Json.Decode.float
        |> Json.Decode.Pipeline.optional "zfar" (Json.Decode.nullable Json.Decode.float) Nothing
        |> Json.Decode.Pipeline.required "znear" Json.Decode.float
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


cameraTypeDecoder : Json.Decode.Decoder CameraType
cameraTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "perspective" ->
                        Json.Decode.succeed Perspective

                    "orthographic" ->
                        Json.Decode.succeed Orthographic

                    t ->
                        Json.Decode.fail <| "Camera type " ++ t ++ " is unknown"
            )


imageDecoder : Json.Decode.Decoder Image
imageDecoder =
    Json.Decode.succeed Image
        |> Json.Decode.Pipeline.optional "uri" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "mimeType" (Json.Decode.nullable imageMimeTypeDecoder) Nothing
        |> Json.Decode.Pipeline.optional "bufferView" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


imageMimeTypeDecoder : Json.Decode.Decoder ImageMimeType
imageMimeTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\mime ->
                case mime of
                    "image/jpeg" ->
                        Json.Decode.succeed ImageJpeg

                    "image/png" ->
                        Json.Decode.succeed ImagePng

                    m ->
                        Json.Decode.fail <| "Image mime type " ++ m ++ " is unknown"
            )


materialDecoder : Json.Decode.Decoder Material
materialDecoder =
    Json.Decode.succeed Material
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing
        |> Json.Decode.Pipeline.optional "pbrMetallicRoughness" (Json.Decode.nullable materialPbrMetallicRoughnessDecoder) Nothing
        |> Json.Decode.Pipeline.optional "normalTexture" (Json.Decode.nullable materialNormalTextureInfoDecoder) Nothing
        |> Json.Decode.Pipeline.optional "occlusionTexture" (Json.Decode.nullable materialOcclusionTextureInfoDecoder) Nothing
        |> Json.Decode.Pipeline.optional "emissiveTexture" (Json.Decode.nullable textureInfoDecoder) Nothing
        |> Json.Decode.Pipeline.optional "emissiveFactor" tripleDecoder ( 0, 0, 0 )
        |> Json.Decode.Pipeline.optional "alphaMode" materialAlphaModeDecoder Opaque
        |> Json.Decode.Pipeline.optional "alphaCutoff" Json.Decode.float 0.5
        |> Json.Decode.Pipeline.optional "doubleSided" Json.Decode.bool False


materialPbrMetallicRoughnessDecoder : Json.Decode.Decoder MaterialPbrMetallicRoughness
materialPbrMetallicRoughnessDecoder =
    Json.Decode.succeed MaterialPbrMetallicRoughness
        |> Json.Decode.Pipeline.optional "baseColorFactor" quaternionDecoder (Quaternion 1 1 1 1)
        |> Json.Decode.Pipeline.optional "baseColorTexture" (Json.Decode.nullable textureInfoDecoder) Nothing
        |> Json.Decode.Pipeline.optional "metallicFactor" Json.Decode.float 1
        |> Json.Decode.Pipeline.optional "roughnessFactor" Json.Decode.float 1
        |> Json.Decode.Pipeline.optional "metallicRoughnessTexture" (Json.Decode.nullable textureInfoDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


materialNormalTextureInfoDecoder : Json.Decode.Decoder MaterialNormalTextureInfo
materialNormalTextureInfoDecoder =
    Json.Decode.succeed MaterialNormalTextureInfo
        |> Json.Decode.Pipeline.required "index" Json.Decode.int
        |> Json.Decode.Pipeline.optional "textCoord" Json.Decode.int 0
        |> Json.Decode.Pipeline.optional "scale" Json.Decode.float 1
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


materialOcclusionTextureInfoDecoder : Json.Decode.Decoder MaterialOcclusionTextureInfo
materialOcclusionTextureInfoDecoder =
    Json.Decode.succeed MaterialOcclusionTextureInfo
        |> Json.Decode.Pipeline.required "index" Json.Decode.int
        |> Json.Decode.Pipeline.optional "textCoord" Json.Decode.int 0
        |> Json.Decode.Pipeline.optional "strength" Json.Decode.float 1
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


textureInfoDecoder : Json.Decode.Decoder TextureInfo
textureInfoDecoder =
    Json.Decode.succeed TextureInfo
        |> Json.Decode.Pipeline.required "index" Json.Decode.int
        |> Json.Decode.Pipeline.optional "textCoord" Json.Decode.int 0
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


materialAlphaModeDecoder : Json.Decode.Decoder MaterialAlphaMode
materialAlphaModeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\mode ->
                case mode of
                    "OPAQUE" ->
                        Json.Decode.succeed Opaque

                    "MASK" ->
                        Json.Decode.succeed Mask

                    "BLEND" ->
                        Json.Decode.succeed Blend

                    m ->
                        Json.Decode.fail <| "Material alpha mode " ++ m ++ " is unknown"
            )


mesheDecoder : Json.Decode.Decoder Mesh
mesheDecoder =
    Json.Decode.succeed Mesh
        |> Json.Decode.Pipeline.required "primitives" (Json.Decode.array meshPrimitiveDecoder)
        |> Json.Decode.Pipeline.optional "weights" (Json.Decode.array Json.Decode.float) Array.empty
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


meshPrimitiveDecoder : Json.Decode.Decoder MeshPrimitive
meshPrimitiveDecoder =
    Json.Decode.succeed MeshPrimitive
        |> Json.Decode.Pipeline.required "attributes" meshPrimitiveAttributeDecoder
        |> Json.Decode.Pipeline.optional "indices" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "material" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "mode" meshPrimitiveModeDecoder Triangles
        |> Json.Decode.Pipeline.optional "targets" (Json.Decode.array Json.Decode.int) Array.empty
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


meshPrimitiveAttributeDecoder : Json.Decode.Decoder MeshPrimitiveAttributes
meshPrimitiveAttributeDecoder =
    Json.Decode.succeed MeshPrimitiveAttributes


meshPrimitiveModeDecoder : Json.Decode.Decoder MeshPrimitiveMode
meshPrimitiveModeDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\mode ->
                case mode of
                    0 ->
                        Json.Decode.succeed Points

                    1 ->
                        Json.Decode.succeed Lines

                    2 ->
                        Json.Decode.succeed LineLoop

                    3 ->
                        Json.Decode.succeed LineStrip

                    4 ->
                        Json.Decode.succeed Triangles

                    5 ->
                        Json.Decode.succeed TriangleStrip

                    6 ->
                        Json.Decode.succeed TriangleFan

                    m ->
                        Json.Decode.fail <| "Material alpha mode " ++ String.fromInt m ++ " is unknown"
            )


nodeDecoder : Json.Decode.Decoder Node
nodeDecoder =
    Json.Decode.succeed Node
        |> Json.Decode.Pipeline.optional "camera" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "children" (Json.Decode.array Json.Decode.int) Array.empty
        |> Json.Decode.Pipeline.optional "skin" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "matrix" matrixDecoder (Matrix4x4 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)
        |> Json.Decode.Pipeline.optional "mesh" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "rotation" quaternionDecoder (Quaternion 0 0 0 1)
        |> Json.Decode.Pipeline.optional "scale" tripleDecoder ( 1, 1, 1 )
        |> Json.Decode.Pipeline.optional "translation" tripleDecoder ( 0, 0, 0 )
        |> Json.Decode.Pipeline.optional "weights" (Json.Decode.array Json.Decode.float) Array.empty
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


samplerDecoder : Json.Decode.Decoder Sampler
samplerDecoder =
    Json.Decode.succeed Sampler
        |> Json.Decode.Pipeline.optional "magFilter" (Json.Decode.nullable samplerMagFilterDecoder) Nothing
        |> Json.Decode.Pipeline.optional "minFilter" (Json.Decode.nullable samplerMinFilterDecoder) Nothing
        |> Json.Decode.Pipeline.optional "wrapS" samplerWrapDecoder Repeat
        |> Json.Decode.Pipeline.optional "wrapT" samplerWrapDecoder Repeat
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


samplerMagFilterDecoder : Json.Decode.Decoder SamplerMagFilter
samplerMagFilterDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\mag ->
                case mag of
                    9728 ->
                        Json.Decode.succeed MagNearest

                    9729 ->
                        Json.Decode.succeed MagLinear

                    m ->
                        Json.Decode.fail <| "Sampler mag filter " ++ String.fromInt m ++ " is unknown"
            )


samplerMinFilterDecoder : Json.Decode.Decoder SamplerMinFilter
samplerMinFilterDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\min ->
                case min of
                    9728 ->
                        Json.Decode.succeed MinNearest

                    9729 ->
                        Json.Decode.succeed MinLinear

                    9984 ->
                        Json.Decode.succeed NearestMipmapNearest

                    9985 ->
                        Json.Decode.succeed LinearMipmapNearest

                    9986 ->
                        Json.Decode.succeed NearestMipmapLinear

                    9987 ->
                        Json.Decode.succeed LinearMipmapLinear

                    m ->
                        Json.Decode.fail <| "Sampler min filter " ++ String.fromInt m ++ " is unknown"
            )


samplerWrapDecoder : Json.Decode.Decoder SamplerWrap
samplerWrapDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\wrap ->
                case wrap of
                    33071 ->
                        Json.Decode.succeed ClampToEdge

                    33648 ->
                        Json.Decode.succeed MirroredRepeat

                    10497 ->
                        Json.Decode.succeed Repeat

                    w ->
                        Json.Decode.fail <| "Sampler wrap " ++ String.fromInt w ++ " is unknown"
            )


sceneDecoder : Json.Decode.Decoder Scene
sceneDecoder =
    Json.Decode.succeed Scene
        |> Json.Decode.Pipeline.optional "nodes" (Json.Decode.array Json.Decode.int) Array.empty
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


skinDecoder : Json.Decode.Decoder Skin
skinDecoder =
    Json.Decode.succeed Skin
        |> Json.Decode.Pipeline.optional "inverseBindMatrices" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "skeleton" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.required "joints" (Json.Decode.array Json.Decode.int)
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


textureDecoder : Json.Decode.Decoder Texture
textureDecoder =
    Json.Decode.succeed Texture
        |> Json.Decode.Pipeline.optional "sampler" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "source" (Json.Decode.nullable Json.Decode.int) Nothing
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.nullable Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "extensions" (Json.Decode.nullable extensionDecoder) Nothing
        |> Json.Decode.Pipeline.optional "extras" (Json.Decode.nullable extraDecoder) Nothing


extensionDecoder : Json.Decode.Decoder Extension
extensionDecoder =
    Json.Decode.succeed Extension


extraDecoder : Json.Decode.Decoder Extra
extraDecoder =
    Json.Decode.succeed Extra


type alias Gltf =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-gltf
    { extensionsUsed : Array String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_extensionsused
    , extensionsRequired : Array String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_extensionsrequired
    , accessors : Array Accessor -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_accessors
    , animations : Array Animation -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_animations
    , asset : Asset -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_asset
    , buffers : Array Buffer -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_buffers
    , bufferViews : Array BufferView -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_bufferviews
    , cameras : Array Camera -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_cameras
    , images : Array Image -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_images
    , materials : Array Material -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_materials
    , meshes : Array Mesh -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_meshes
    , nodes : Array Node -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_nodes
    , samplers : Array Sampler -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_samplers
    , scene : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_scene
    , scenes : Array Scene -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_scenes
    , skins : Array Skin -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_skins
    , textures : Array Texture -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_textures
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_gltf_extras
    }


type alias Accessor =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-accessor
    { bufferView : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_bufferview
    , byteOffset : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_byteoffset
    , componentType : AccessorComponentType -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_componenttype
    , normalized : Bool -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_normalized
    , count : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_count
    , type_ : AccessorType -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_type
    , max : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_max
    , min : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_min
    , sparse : Maybe AccessorSparse -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_extras
    }


type AccessorComponentType
    = Byte -- 8 bits
    | UnsignedByte -- 8 bits
    | Short -- 16 bits
    | UnsignedShort -- 16 bits
    | UnsignedInt -- 32 bits
    | Float -- 32 bits


type AccessorType
    = Scalar -- 1 component
    | Vec2 -- 2 component
    | Vec3 -- 3 component
    | Vec4 -- 4 component
    | Mat2 -- 4 component
    | Mat3 -- 9 component
    | Mat4 -- 16 component


type alias AccessorSparse =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-accessor-sparse
    { count : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_count
    , indices : AccessorSparseIndices -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_indices
    , values : AccessorSparseValues -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_values
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_extras
    }


type alias AccessorSparseIndices =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-accessor-sparse-indices
    { bufferView : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_indices_bufferview
    , byteOffset : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_indices_byteoffset
    , componentType : AccessorSparseIndicesComponentType -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_indices_componenttype
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_indices_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_indices_extras
    }


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_indices_componenttype>
-}
type AccessorSparseIndicesComponentType
    = IndicesUnsignedByte -- 8 bits
    | IndicesUnsignedShort -- 16 bits
    | IndicesUnsignedInt -- 32 bits


type alias AccessorSparseValues =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-accessor-sparse-values
    { bufferView : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_values_bufferview
    , byteOffset : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_values_byteoffset
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_values_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_accessor_sparse_values_extras
    }


type alias Animation =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-animation
    { channels : Array AnimationChannel -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channels
    , samplers : Array AnimationSampler -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_samplers
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_extras
    }


type alias AnimationChannel =
    --https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-animation-channel
    { sampler : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_sampler
    , target : AnimationChannelTarget -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_target
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_extras
    }


type alias AnimationChannelTarget =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-animation-channel-target
    { node : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_target_node
    , path : AnimationChannelTargetPath -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_target_path
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_target_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_target_extras
    }


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_channel_target_path>
-}
type AnimationChannelTargetPath
    = Translation
    | Rotation
    | Scale
    | Weights


type alias AnimationSampler =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-animation-sampler
    { input : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_sampler_input
    , interpolation : AnimationSamplerInterpolation -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_sampler_interpolation
    , output : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_sampler_output
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_sampler_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_sampler_extras
    }


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_animation_sampler_interpolation>
-}
type AnimationSamplerInterpolation
    = Linear
    | Step
    | CubicSpline


type alias Asset =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-asset
    { copyright : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_asset_copyright
    , generator : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_asset_generator
    , version : String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_asset_version
    , minVersion : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_asset_minversion
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_asset_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_asset_extras
    }


type alias Buffer =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-buffer
    { uri : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_buffer_uri
    , byteLength : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_buffer_bytelength
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_buffer_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_buffer_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_buffer_extras
    }


type alias BufferView =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-bufferview
    { buffer : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_buffer
    , byteOffset : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_byteoffset
    , byteLength : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_bytelength
    , byteStride : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_bytestride
    , target : Maybe BufferViewTarget -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_target
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_extras
    }


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_target>
-}
type BufferViewTarget
    = ArrayBuffer -- 34962
    | ElementArrayBuffer -- 34963


type alias Camera =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-camera
    { orthographic : Maybe CameraOrthographic -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_orthographic
    , perspective : Maybe CameraPerspective -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_perspective
    , type_ : CameraType -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_type
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_extras
    }


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_type>
-}
type CameraType
    = Perspective
    | Orthographic


type alias CameraOrthographic =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-camera-orthographic
    { xmag : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_orthographic_xmag
    , ymag : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_orthographic_ymag
    , zfar : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_orthographic_zfar
    , znear : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_orthographic_znear
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_orthographic_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_orthographic_extras
    }


type alias CameraPerspective =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-camera-perspective
    { aspectRatio : Maybe Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_perspective_aspectratio
    , yfov : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_perspective_yfov
    , zfar : Maybe Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_perspective_zfar
    , znear : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_perspective_znear
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_perspective_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_camera_perspective_extras
    }


type alias Extension =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-extension
    {}


type alias Extra =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-extras
    {}


type alias Image =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-image
    { uri : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_image_uri
    , mimeType : Maybe ImageMimeType -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_image_mimetype
    , bufferView : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_image_bufferview
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_image_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_image_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_image_extras
    }


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_image_mimetype>
-}
type ImageMimeType
    = ImageJpeg
    | ImagePng


type alias Material =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-material
    { name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_extras
    , pbrMetallicRoughness : Maybe MaterialPbrMetallicRoughness -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_pbrmetallicroughness
    , normalTexture : Maybe MaterialNormalTextureInfo -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_normaltexture
    , occlusionTexture : Maybe MaterialOcclusionTextureInfo -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_occlusiontexture
    , emissiveTexture : Maybe TextureInfo -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_emissivetexture
    , emissiveFactor : ( Float, Float, Float ) -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_emissivefactor
    , alphaMode : MaterialAlphaMode -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_alphamode
    , alphaCutoff : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_alphacutoff
    , doubleSided : Bool -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_doublesided
    }


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_alphamode>
-}
type MaterialAlphaMode
    = Opaque
    | Mask
    | Blend


type alias MaterialNormalTextureInfo =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-material-normaltextureinfo
    { index : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_normaltextureinfo_index
    , texCoord : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_normaltextureinfo_texcoord
    , scale : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_normaltextureinfo_scale
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_normaltextureinfo_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_normaltextureinfo_extras
    }


type alias MaterialOcclusionTextureInfo =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-material-occlusiontextureinfo
    { index : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_occlusiontextureinfo_index
    , texCoord : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_occlusiontextureinfo_texcoord
    , strength : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_occlusiontextureinfo_strength
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_occlusiontextureinfo_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_occlusiontextureinfo_extras
    }


type alias MaterialPbrMetallicRoughness =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-material-pbrmetallicroughness
    { baseColorFactor : Quaternion -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_pbrmetallicroughness_basecolorfactor
    , baseColorTexture : Maybe TextureInfo -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_pbrmetallicroughness_basecolortexture
    , metallicFactor : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_pbrmetallicroughness_metallicfactor
    , roughnessFactor : Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_pbrmetallicroughness_roughnessfactor
    , metallicRoughnessTexture : Maybe TextureInfo -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_pbrmetallicroughness_metallicroughnesstexture
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_pbrmetallicroughness_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_material_pbrmetallicroughness_extras
    }


type alias Mesh =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-mesh
    { primitives : Array MeshPrimitive -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitives
    , weights : Array Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_weights
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_extras
    }


type alias MeshPrimitive =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-mesh-primitive
    { attributes : MeshPrimitiveAttributes -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_attributes
    , indices : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_indices
    , material : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_material
    , mode : MeshPrimitiveMode -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_mode
    , targets : Array Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_targets
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_extras
    }


type alias MeshPrimitiveAttributes =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_attributes
    {}


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_mode>
-}
type MeshPrimitiveMode
    = Points -- 0
    | Lines -- 1
    | LineLoop -- 2
    | LineStrip -- 3
    | Triangles -- 4
    | TriangleStrip -- 5
    | TriangleFan -- 6


type alias Node =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-node
    { camera : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_camera
    , children : Array Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_children
    , skin : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_skin
    , matrix : Matrix4x4 -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_matrix
    , mesh : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_mesh
    , rotation : Quaternion -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_rotation
    , scale : ( Float, Float, Float ) -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_scale
    , translation : ( Float, Float, Float ) -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_translation
    , weights : Array Float -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_weights
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_node_extras
    }


type alias Sampler =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-sampler
    { magFilter : Maybe SamplerMagFilter -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_magfilter
    , minFilter : Maybe SamplerMinFilter -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_minfilter
    , wrapS : SamplerWrap -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_wraps
    , wrapT : SamplerWrap -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_wrapt
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_extras
    }


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_magfilter>
-}
type SamplerMagFilter
    = MagNearest -- 9728
    | MagLinear -- 9729


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_minfilter>
-}
type SamplerMinFilter
    = MinNearest -- 9728
    | MinLinear -- 9729
    | NearestMipmapNearest -- 9984
    | LinearMipmapNearest -- 9985
    | NearestMipmapLinear -- 9986
    | LinearMipmapLinear -- 9987


{-| <https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_sampler_wraps>
-}
type SamplerWrap
    = ClampToEdge -- 33071
    | MirroredRepeat -- 33648
    | Repeat -- 10497


type alias Scene =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-scene
    { nodes : Array Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_scene_nodes
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_scene_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_scene_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_scene_extras
    }


type alias Skin =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-skin
    { inverseBindMatrices : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_skin_inversebindmatrices
    , skeleton : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_skin_skeleton
    , joints : Array Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_skin_joints
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_skin_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_skin_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_skin_extras
    }


type alias Texture =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-texture
    { sampler : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_texture_sampler
    , source : Maybe Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_texture_source
    , name : Maybe String -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_texture_name
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_texture_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_texture_extras
    }


type alias TextureInfo =
    -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-textureinfo
    { index : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_textureinfo_index
    , texCoord : Int -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_textureinfo_texcoord
    , extensions : Maybe Extension -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_textureinfo_extensions
    , extras : Maybe Extra -- https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_textureinfo_extras
    }
