{-# LANGUAGE PackageImports, LambdaCase, OverloadedStrings #-}

module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Data.Map as Map
import qualified Data.Vector as V

import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL

import Codec.Picture as Juicy

import Data.Aeson
import qualified Data.ByteString as SB

main :: IO ()
main = do
    win <- initWindow "LambdaCube 3D DSL Hello World" 2000 2000
    let inputSchema = makeSchema $ do
            defObjectArray "objects" Triangles $ do
                "position"  @: Attribute_V2F
                "uv"        @: Attribute_V2F
            defUniforms $ do
                "time"           @: Float
                "diffuseTexture" @: FTexture2D

    storage <- LambdaCubeGL.allocStorage inputSchema
    LambdaCubeGL.uploadMeshToGPU triangleA >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []
    LambdaCubeGL.uploadMeshToGPU triangleB >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []

    Right img <- Juicy.readImage "media/logo.jpg"
    textureData <- LambdaCubeGL.uploadTexture2DToGPU img

    Just pipelineDesc <- decodeStrict <$> SB.readFile "app/hello.json"
    renderer <- LambdaCubeGL.allocRenderer pipelineDesc

    LambdaCubeGL.setStorage renderer storage >>= \case
        Just err -> putStrLn err
        Nothing  -> loop
            where loop = do
                    (w, h) <- GLFW.getWindowSize win
                    LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
                    LambdaCubeGL.updateUniforms storage $ do
                        "diffuseTexture" @= return textureData
                        "time" @= do
                            Just t <- GLFW.getTime
                            return (realToFrac t :: Float)
                    LambdaCubeGL.renderFrame renderer
                    GLFW.swapBuffers win
                    GLFW.pollEvents

                    let keyIsPressed k = fmap (==KeyState'Pressed) (GLFW.getKey win k)
                    escape <- keyIsPressed Key'Escape
                    if escape then return () else loop

    LambdaCubeGL.disposeRenderer renderer
    LambdaCubeGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate

triangleA :: LambdaCubeGL.Mesh
triangleA = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 1 1, V2 1 (-1), V2 (-1) (-1)])
        , ("uv",        A_V2F $ V.fromList [V2 1 1, V2 0 1, V2 0 0])
        ]
    , mPrimitive    = P_Triangles
    }

triangleB :: LambdaCubeGL.Mesh
triangleB = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 1 1, V2 (-1) (-1), V2 (-1) 1])
        , ("uv",        A_V2F $ V.fromList [V2 1 1, V2 0 0, V2 1 0])
        ]
    , mPrimitive    = P_Triangles
    }

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
        [ WindowHint'ContextVersionMajor 3
        , WindowHint'ContextVersionMinor 3
        , WindowHint'OpenGLProfile OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat True
        ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win
