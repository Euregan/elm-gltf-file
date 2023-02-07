module Gltf.Decode.Mesh exposing (texturedFacesFromDefaultScene)

import Array
import Bytes exposing (Bytes)
import Bytes.Decode
import Gltf.Decode.Json.Raw as Raw
import Json.Decode
import Length exposing (Meters)
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


getMeshes : Raw.Gltf -> Raw.Node -> Result String (List Raw.Mesh)
getMeshes gltf node =
    case ( node.mesh, node.children ) of
        ( Just meshIndex, _ ) ->
            case Array.get meshIndex gltf.meshes of
                Just mesh ->
                    Ok [ mesh ]

                Nothing ->
                    Err <| "There is no mesh at index " ++ String.fromInt meshIndex ++ ", as the mesh array is only " ++ (String.fromInt <| Array.length gltf.meshes) ++ " items long"

        ( _, [] ) ->
            Err <| "The node " ++ (node.name |> Maybe.map (\name -> name ++ " ") |> Maybe.withDefault "") ++ "has no mesh and no children"

        ( _, children ) ->
            children
                |> List.map (getNode gltf)
                |> List.map (Result.andThen <| getMeshes gltf)
                |> resultFromList
                |> Result.map List.concat


getVec3 : Bytes.Decode.Decoder a -> Int -> Raw.Gltf -> Bytes -> Int -> Result String (List ( a, a, a ))
getVec3 scalarDecoder loopOffset gltf bytes accessorIndex =
    case Array.get accessorIndex gltf.accessors of
        Just accessor ->
            case accessor.bufferView of
                Just bufferViewIndex ->
                    case Array.get bufferViewIndex gltf.bufferViews of
                        Just bufferView ->
                            let
                                raw =
                                    Bytes.Decode.decode
                                        (skipBytes
                                            (accessor.byteOffset + bufferView.byteOffset)
                                            (Bytes.Decode.loop ( 0, [] )
                                                (\( done, vertices ) ->
                                                    if done < accessor.count then
                                                        Bytes.Decode.map3 (\x y z -> ( x, y, z ))
                                                            scalarDecoder
                                                            scalarDecoder
                                                            scalarDecoder
                                                            |> Bytes.Decode.map (\vector -> Bytes.Decode.Loop ( done + loopOffset, vector :: vertices ))

                                                    else
                                                        Bytes.Decode.Done vertices
                                                            |> Bytes.Decode.succeed
                                                )
                                            )
                                        )
                                        bytes
                            in
                            case raw of
                                Just positions ->
                                    Ok positions

                                Nothing ->
                                    Err <| "Could not fetch accessor " ++ String.fromInt accessorIndex ++ " VEC3 data"

                        Nothing ->
                            Err <| "There is no buffer view at index " ++ String.fromInt bufferViewIndex ++ ", as the buffer view array is only " ++ (String.fromInt <| Array.length gltf.bufferViews) ++ " items long"

                Nothing ->
                    Err <| "The accessor at index " ++ String.fromInt accessorIndex ++ " has no buffer view index"

        Nothing ->
            Err <| "There is no accessor at index " ++ String.fromInt accessorIndex ++ ", as the accessor array is only " ++ (String.fromInt <| Array.length gltf.accessors) ++ " items long"


getFloatVec3 : Raw.Gltf -> Bytes -> Int -> Result String (List ( Float, Float, Float ))
getFloatVec3 =
    getVec3 (Bytes.Decode.float32 Bytes.LE) 1


getIntVec3 : Raw.Gltf -> Bytes -> Int -> Result String (List ( Int, Int, Int ))
getIntVec3 =
    getVec3 (Bytes.Decode.unsignedInt32 Bytes.LE) 3


getPrimitiveAttributes : Raw.Gltf -> Bytes -> Raw.Mesh -> Int -> Raw.MeshPrimitive -> ( Result String (List (Point3d Meters coordinates)), Result String (List (Vector3d Unitless coordinates)), Result String (List ( Int, Int, Int )) )
getPrimitiveAttributes gltf bytes mesh primitiveIndex primitive =
    ( case primitive.attributes.position of
        Just positionIndex ->
            getFloatVec3 gltf bytes positionIndex
                |> Result.map (List.map (\( x, y, z ) -> Point3d.meters x y z))

        Nothing ->
            Err <| "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no position"
    , case primitive.attributes.normal of
        Just normalIndex ->
            getFloatVec3 gltf bytes normalIndex
                |> Result.map (List.map (\( x, y, z ) -> Vector3d.unitless x y z))

        Nothing ->
            Err <| "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no normal"
    , case primitive.indices of
        Just indicesIndex ->
            getIntVec3 gltf bytes indicesIndex

        Nothing ->
            Err <| "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no indices"
    )


toTriangularMesh : Raw.Gltf -> Bytes -> Raw.Mesh -> Result String (TriangularMesh (Vertex coordinates))
toTriangularMesh gltf bytes mesh =
    List.indexedMap (getPrimitiveAttributes gltf bytes mesh) mesh.primitives
        |> List.map
            (\( positionsResult, normalsResult, indicesResult ) ->
                Result.map3
                    (\positions normals indices ->
                        ( List.map2
                            (\position normal ->
                                { position = position
                                , normal = normal
                                , uv = ( 0, 0 )
                                }
                            )
                            (positions |> List.reverse)
                            (normals |> List.reverse)
                            |> Array.fromList
                        , indices
                        )
                    )
                    positionsResult
                    normalsResult
                    indicesResult
            )
        |> List.map (Result.map (\( vertices, indices ) -> TriangularMesh.indexed vertices indices))
        |> resultFromList
        |> Result.map TriangularMesh.combine


skipBytes : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder a
skipBytes skip decoder =
    Bytes.Decode.bytes skip |> Bytes.Decode.andThen (\_ -> decoder)


texturedFacesFromDefaultScene : Bytes -> Result String (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) })
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
                                |> List.map (Result.map TriangularMesh.combine)
                                |> resultFromList
                                |> Result.map TriangularMesh.combine

                        Nothing ->
                            Err <| "The default scene index (" ++ String.fromInt sceneIndex ++ ") is out of bound of the array of scenes (" ++ (String.fromInt <| Array.length gltf.scenes) ++ " items)"

                Nothing ->
                    Err "There is no default scene in the file"

        Nothing ->
            Err "The file is malformed"
