module DecodeJsonRaw exposing (..)

import Expect
import Gltf.Decode.Json.Raw exposing (decoder)
import Json.Decode
import Resources
import Test exposing (..)


suite : Test
suite =
    describe "Decoding the JSON part of the GLTF format"
        [ test "works on a small generic model" <|
            \_ ->
                let
                    gltf =
                        Json.Decode.decodeString decoder Resources.skeleton.string
                in
                Expect.equal (Ok Resources.skeleton.raw) gltf
        ]
