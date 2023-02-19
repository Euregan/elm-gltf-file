module Gltf.Decode.Mesh exposing (texturedFacesFromDefaultScene)

import Array
import Bytes exposing (Bytes)
import Bytes.Decode
import Color exposing (Color)
import Gltf.Decode.Json.Raw as Raw
import Json.Decode
import Length exposing (Meters)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


type alias Vertex coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }


resultFromList : List (Result error a) -> Result error (List a)
resultFromList =
    List.foldl
        (\result listResult ->
            case ( result, listResult ) of
                ( Err error, _ ) ->
                    Err error

                ( _, Err error ) ->
                    Err error

                ( Ok element, Ok list ) ->
                    Ok <| element :: list
        )
        (Ok [])


getNode : Raw.Gltf -> Int -> Result String Raw.Node
getNode gltf nodeIndex =
    case Array.get nodeIndex gltf.nodes of
        Just node ->
            Ok node

        Nothing ->
            Err <| "There is no node at index " ++ String.fromInt nodeIndex ++ ", as the node array is only " ++ (String.fromInt <| Array.length gltf.nodes) ++ " items long"


getMeshes : Raw.Gltf -> Raw.Node -> Result String (List ( Raw.Mesh, Mat4 ))
getMeshes gltf node =
    case ( node.mesh, node.children ) of
        ( Just meshIndex, _ ) ->
            case Array.get meshIndex gltf.meshes of
                Just mesh ->
                    Ok [ ( mesh, node.matrix ) ]

                Nothing ->
                    Err <| "There is no mesh at index " ++ String.fromInt meshIndex ++ ", as the mesh array is only " ++ (String.fromInt <| Array.length gltf.meshes) ++ " items long"

        ( _, [] ) ->
            Err <| "The node " ++ (node.name |> Maybe.map (\name -> name ++ " ") |> Maybe.withDefault "") ++ "has no mesh and no children"

        ( _, children ) ->
            children
                |> List.map (getNode gltf)
                |> List.map (Result.andThen <| getMeshes gltf)
                |> List.map (Result.map (List.map <| \( mesh, modifier ) -> ( mesh, Math.Matrix4.mul modifier node.matrix )))
                |> resultFromList
                |> Result.map List.concat


mapAccessor : (Raw.Accessor -> Result String (Bytes.Decode.Decoder a)) -> Raw.Gltf -> Bytes -> Int -> Result String a
mapAccessor decoderConstructor gltf bytes accessorIndex =
    case Array.get accessorIndex gltf.accessors of
        Just accessor ->
            Result.andThen
                (\decoder ->
                    case accessor.bufferView of
                        Just bufferViewIndex ->
                            case Array.get bufferViewIndex gltf.bufferViews of
                                Just bufferView ->
                                    let
                                        raw =
                                            Bytes.Decode.decode
                                                (skipBytes
                                                    (accessor.byteOffset + bufferView.byteOffset)
                                                    decoder
                                                )
                                                bytes
                                    in
                                    case raw of
                                        Just result ->
                                            Ok result

                                        Nothing ->
                                            Err <| "Could not fetch accessor " ++ String.fromInt accessorIndex ++ " VEC3 data"

                                Nothing ->
                                    Err <| "There is no buffer view at index " ++ String.fromInt bufferViewIndex ++ ", as the buffer view array is only " ++ (String.fromInt <| Array.length gltf.bufferViews) ++ " items long"

                        Nothing ->
                            Err <| "The accessor at index " ++ String.fromInt accessorIndex ++ " has no buffer view index"
                )
            <|
                decoderConstructor accessor

        Nothing ->
            Err <| "There is no accessor at index " ++ String.fromInt accessorIndex ++ ", as the accessor array is only " ++ (String.fromInt <| Array.length gltf.accessors) ++ " items long"


loop : Int -> Bytes.Decode.Decoder a -> Int -> Bytes.Decode.Decoder (List ( a, a, a ))
loop increment decoder count =
    Bytes.Decode.loop ( 0, [] )
        (\( done, vertices ) ->
            if done < count then
                Bytes.Decode.map3 (\x y z -> ( x, y, z ))
                    decoder
                    decoder
                    decoder
                    |> Bytes.Decode.map (\vector -> Bytes.Decode.Loop ( done + increment, vector :: vertices ))

            else
                Bytes.Decode.Done vertices
                    |> Bytes.Decode.succeed
        )


getFloatVec3 : Raw.Gltf -> Bytes -> Int -> Result String (List ( Float, Float, Float ))
getFloatVec3 =
    mapAccessor
        (\accessor ->
            let
                decoderResult =
                    case accessor.componentType of
                        Raw.Float ->
                            Ok <| Bytes.Decode.float32 Bytes.LE

                        _ ->
                            Err <| "Accessor" ++ (accessor.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ " was expected to point to a float"
            in
            decoderResult
                |> Result.map
                    (\decoder ->
                        loop 1 decoder accessor.count
                    )
        )


getIntVec3 : Raw.Gltf -> Bytes -> Int -> Result String (List ( Int, Int, Int ))
getIntVec3 =
    mapAccessor
        (\accessor ->
            let
                decoderResult =
                    case accessor.componentType of
                        Raw.UnsignedShort ->
                            Ok <| Bytes.Decode.unsignedInt16 Bytes.LE

                        Raw.UnsignedInt ->
                            Ok <| Bytes.Decode.unsignedInt32 Bytes.LE

                        _ ->
                            Err <| "Accessor" ++ (accessor.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ " was expected to point to an integer"
            in
            decoderResult
                |> Result.map
                    (\decoder ->
                        loop 3 decoder accessor.count
                    )
        )


getPositions : Raw.Gltf -> Bytes -> Raw.MeshPrimitive -> Result String (List (Point3d Meters coordinates))
getPositions gltf bytes primitive =
    case primitive.attributes.position of
        Just positionIndex ->
            getFloatVec3 gltf bytes positionIndex
                |> Result.map (List.map (\( x, y, z ) -> Point3d.meters x y z))

        Nothing ->
            Err <| "No positions were found on this primitive"


getNormals : Raw.Gltf -> Bytes -> Raw.MeshPrimitive -> Result String (List (Vector3d Unitless coordinates))
getNormals gltf bytes primitive =
    case primitive.attributes.normal of
        Just normalIndex ->
            getFloatVec3 gltf bytes normalIndex
                |> Result.map (List.map (\( x, y, z ) -> Vector3d.unitless x y z))

        Nothing ->
            Err <| "No normals were found on this primitive"


getIndices : Raw.Gltf -> Bytes -> Raw.MeshPrimitive -> Result String (List ( Int, Int, Int ))
getIndices gltf bytes primitive =
    case primitive.indices of
        Just indicesIndex ->
            getIntVec3 gltf bytes indicesIndex

        Nothing ->
            Err <| "No indice were found on this primitive"


getColor : Raw.Gltf -> Raw.MeshPrimitive -> Result String Color
getColor gltf primitive =
    case primitive.material of
        Just materialIndex ->
            case Array.get materialIndex gltf.materials of
                Just material ->
                    case material.pbrMetallicRoughness of
                        Just pbr ->
                            Ok <| Color.fromRgba pbr.baseColorFactor

                        Nothing ->
                            Err <| "no PBR metallic roughness was found on material" ++ (material.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ " at index " ++ String.fromInt materialIndex

                Nothing ->
                    Err <| "No material was found at index " ++ String.fromInt materialIndex ++ " (There are only " ++ String.fromInt (Array.length gltf.materials) ++ " materials)"

        Nothing ->
            Err "No material was found on this primitive"


type alias Primitive coordinates =
    { positions : List (Point3d Meters coordinates)
    , normals : List (Vector3d Unitless coordinates)
    , indices : List ( Int, Int, Int )
    , color : Color
    }


getPrimitiveAttributes : Raw.Gltf -> Bytes -> Raw.Mesh -> Int -> Raw.MeshPrimitive -> Result String (Primitive coordinates)
getPrimitiveAttributes gltf bytes mesh primitiveIndex primitive =
    Result.map4 Primitive
        (getPositions gltf bytes primitive
            |> Result.mapError (\_ -> "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no positions")
        )
        (getNormals gltf bytes primitive
            |> Result.mapError (\_ -> "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no normals")
        )
        (getIndices gltf bytes primitive
            |> Result.mapError (\_ -> "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no indices")
        )
        (getColor gltf primitive
            |> Result.mapError (\_ -> "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no color specified in its material")
        )


toTriangularMesh : Raw.Gltf -> Bytes -> ( Raw.Mesh, Mat4 ) -> Result String (List ( TriangularMesh (Vertex coordinates), Color ))
toTriangularMesh gltf bytes ( mesh, modifier ) =
    List.indexedMap (getPrimitiveAttributes gltf bytes mesh) mesh.primitives
        |> List.map
            (Result.map
                (\{ positions, normals, indices, color } ->
                    ( TriangularMesh.indexed
                        (List.map2
                            (\position normal ->
                                { position =
                                    position
                                        |> Point3d.unwrap
                                        |> Math.Vector3.fromRecord
                                        |> Math.Matrix4.transform modifier
                                        |> Math.Vector3.toRecord
                                        |> Point3d.unsafe
                                , normal =
                                    normal
                                        |> Vector3d.unwrap
                                        |> Math.Vector3.fromRecord
                                        |> Math.Matrix4.transform modifier
                                        |> Math.Vector3.toRecord
                                        |> Vector3d.unsafe
                                , uv = ( 0, 0 )
                                }
                            )
                            (positions |> List.reverse)
                            (normals |> List.reverse)
                            |> Array.fromList
                        )
                        indices
                    , color
                    )
                )
            )
        |> resultFromList


skipBytes : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder a
skipBytes skip decoder =
    Bytes.Decode.bytes skip |> Bytes.Decode.andThen (\_ -> decoder)


texturedFacesFromDefaultScene : Bytes -> Result String (List ( TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) }, Color ))
texturedFacesFromDefaultScene bytes =
    let
        decoder =
            Bytes.Decode.string 4
                |> Bytes.Decode.andThen
                    (\magic ->
                        case magic of
                            "glTF" ->
                                Bytes.Decode.succeed "glTF"

                            _ ->
                                Bytes.Decode.fail
                    )
                |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
                |> Bytes.Decode.andThen
                    (\version ->
                        case version of
                            2 ->
                                Bytes.Decode.succeed 2

                            _ ->
                                Bytes.Decode.fail
                    )
                |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
                |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
                |> Bytes.Decode.andThen
                    (\length ->
                        Bytes.Decode.unsignedInt32 Bytes.LE
                            |> Bytes.Decode.andThen (\_ -> Bytes.Decode.succeed length)
                    )
                |> Bytes.Decode.andThen (\length -> Bytes.Decode.string length)
                |> Bytes.Decode.andThen
                    (\json ->
                        case Json.Decode.decodeString Raw.decoder json of
                            Ok mesh ->
                                Bytes.Decode.unsignedInt32 Bytes.LE
                                    |> Bytes.Decode.andThen
                                        (\length ->
                                            Bytes.Decode.unsignedInt32 Bytes.LE
                                                |> Bytes.Decode.andThen (\_ -> Bytes.Decode.succeed length)
                                        )
                                    |> Bytes.Decode.andThen
                                        (\length ->
                                            Bytes.Decode.bytes length
                                        )
                                    |> Bytes.Decode.andThen (\b -> Bytes.Decode.succeed ( mesh, b ))

                            Err _ ->
                                Bytes.Decode.fail
                    )
                |> Bytes.Decode.decode
    in
    case decoder bytes of
        Just ( gltf, valuesBytes ) ->
            case gltf.scene of
                Just sceneIndex ->
                    case Array.get sceneIndex gltf.scenes of
                        Just scene ->
                            scene.nodes
                                |> Array.toList
                                |> List.map (getNode gltf)
                                |> List.map (Result.andThen (getMeshes gltf))
                                |> List.map (Result.andThen (\meshes -> meshes |> List.map (toTriangularMesh gltf valuesBytes) |> resultFromList))
                                |> List.map (Result.map List.concat)
                                |> resultFromList
                                |> Result.map List.concat

                        Nothing ->
                            Err <| "The default scene index (" ++ String.fromInt sceneIndex ++ ") is out of bound of the array of scenes (" ++ (String.fromInt <| Array.length gltf.scenes) ++ " items)"

                Nothing ->
                    Err "There is no default scene in the file"

        Nothing ->
            Err "The file is malformed"
