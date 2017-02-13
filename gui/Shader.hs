module Shader where

import Data.ByteString.Internal


polyVertexShaderBS :: ByteString
polyVertexShaderBS = packChars polyVertexShader

polyVertexShader :: String
polyVertexShader = unlines
    [ "#version 140"
    , "in vec3 vertexPos;"
    , "in vec3 vertexNormal;"
    , "in vec3 vertexColor;"
    , "uniform mat4 cam;"
    , "uniform mat4 projcam;"
    , "out vec3 pointNormal;"
    , "out vec3 diffuse;"
    , "out vec4 eye;"
    , ""
    , "void main() {"
    , "  gl_Position = projcam * vec4(vertexPos,1);"
    , "  eye = cam * vec4(vertexPos,1);"
    , "  pointNormal = vertexNormal;"
    , "  diffuse = vertexColor;"
    , "}"
    ]

polyFragmentShaderBS :: ByteString
polyFragmentShaderBS = packChars polyFragmentShader

polyFragmentShader :: String
polyFragmentShader = unlines
    [ "#version 140"
    , "in vec3 pointNormal;"
    , "in vec4 eye;           // Direction to the viewer"
    , "in vec3 diffuse;       // Material color"
    , "uniform vec3 lightDir; // Direction to the light"
    , "uniform vec3 lightDir2; // Direction to the light"
    , "out vec4 fragColor;"
    , ""
    , "const vec4 ambient = vec4(0.2, 0.2, 0.2, 1); // Ambient light color"
--    , "const vec4 specular = vec4(0.2,0.2,0.2,1); // Light specular color"
    , "const vec4 specular = vec4(1); // Light specular color"
    , "const float shininess = 150;   // Material property"
    , ""
    , "void main() {"
    , "  vec3 e = normalize(eye.xyz);"
    , "  float intensity = dot(pointNormal, lightDir);"
    , "  vec4 spec = vec4(0.0);"
    , "  float specR = max(0.0, dot(e, pointNormal));"
    , "  spec = specular * pow(specR, shininess);"
    , "  fragColor = vec4(0.65*diffuse,1)+spec;"
    , "}"
    ]


logoVertexShaderBS :: ByteString
logoVertexShaderBS = packChars logoVertexShader

logoVertexShader :: String
logoVertexShader = unlines
    [ "#version 140"
    , "in vec2 vertexCoord;"
    , "out vec2 texCoord;"
    , ""
    , "void main() {"
    , "  texCoord = vec2(vertexCoord.x > 0.0 ? 1.0 : 0.0,"
    , "                  vertexCoord.y > 0.0 ? 0.0 : 1.0);"
    , "  gl_Position = vec4(vertexCoord*2.0-1.0,1,1);"
    , "}"
    ]

logoFragmentShaderBS :: ByteString
logoFragmentShaderBS = packChars logoFragmentShader

logoFragmentShader :: String
logoFragmentShader = unlines
    [ "#version 140"
    , "uniform sampler2D tex;"
    , "in vec2 texCoord;"
    , "out vec4 fragColor;"
    , ""
    , "void main() {"
    , "  fragColor = texture(tex, texCoord);"
    , "}"
    ]


groundVertexShaderBS :: ByteString
groundVertexShaderBS = packChars groundVertexShader

groundVertexShader :: String
groundVertexShader = unlines
    [ "#version 140"
    , "in vec3 vertexPos;"
    , "uniform mat4 projcam;"
    , "out vec2 texCoord;"
    , ""
    , "void main() {"
    , "  texCoord = vertexPos.xz * 0.25f;"
    , "  gl_Position = projcam * vec4(vertexPos, 1.0);"
    , "}"
    ]

groundFragmentShaderBS :: ByteString
groundFragmentShaderBS = packChars groundFragmentShader

groundFragmentShader :: String
groundFragmentShader = unlines
    [ "#version 140"
    , "uniform sampler2D tex;"
    , "in vec2 texCoord;"
    , "out vec4 fragColor;"
    , ""
    , "void main() {"
    , "  fragColor = texture(tex, texCoord);"
    , "  //fragColor = vec4(1.0);"
    , "}"
    ]


phongVertexShaderBS :: ByteString
phongVertexShaderBS = packChars phongVertexShader

phongVertexShader :: String
phongVertexShader = unlines
    [ "#version 140"
    , "in vec3 vertexPos, vertexNormal, vertexColor;"
    , "uniform mat4 cam, projcam;"
    , "out vec3 eyePosition, eyeNormal, diffuse;"
    , ""
    , "void main () {"
    , "    eyePosition = vec3 (cam * vec4 (vertexPos, 1.0));"
    , "    eyeNormal = vec3 (cam * vec4 (vertexNormal, 0.0));"
    , "    gl_Position = projcam * vec4 (eyePosition, 1.0);"
    , "    diffuse = vertexColor;"
    , "}"
    ]

phongFragmentShaderBS :: ByteString
phongFragmentShaderBS = packChars phongFragmentShader

phongFragmentShader :: String
phongFragmentShader = unlines
    [ "#version 140"
    , "in vec3 eyePosition, eyeNormal, diffuse;"
    , "uniform mat4 cam;"
    , "out vec4 fragmentColour; // final colour of surface"
    , ""
    , "// fixed point light properties"
    , "vec3 light_position_world  = vec3 (2.0, 0.0, 2.0);"
    , "vec3 Ls = vec3 (1.0, 1.0, 1.0); // white specular colour"
    , "vec3 Ld = vec3 (0.7, 0.7, 0.7); // dull white diffuse light colour"
    , "vec3 La = vec3 (0.2, 0.2, 0.2); // grey ambient colour"
    , "  "
    , "// surface reflectance"
    , "vec3 Ks = vec3 (1.0, 1.0, 1.0); // fully reflect specular light"
    , "vec3 Kd = vec3 (1.0, 0.5, 0.0); // orange diffuse surface reflectance"
    , "vec3 Ka = vec3 (1.0, 1.0, 1.0); // fully reflect ambient light"
    , "float specular_exponent = 100.0; // specular 'power'"
    , ""
    , "void main () {"
    , "    // ambient intensity"
    , "    vec3 Ia = La * Ka;"
    , ""
    , "    // diffuse intensity"
    , "    // raise light position to eye space"
    , "    vec3 light_position_eye = vec3 (cam * vec4 (light_position_world, 1.0));"
    , "    //vec3 light_position_eye = vec3 (vec4 (light_position_world, 1.0));"
    , "    vec3 distance_to_light_eye = light_position_eye - eyePosition;"
    , "    vec3 direction_to_light_eye = normalize (distance_to_light_eye);"
    , "    float dot_prod = dot (direction_to_light_eye, eyeNormal);"
    , "    dot_prod = max (dot_prod, 0.0);"
    , "    vec3 Id = Ld * Kd * dot_prod; // final diffuse intensity"
    , "    Id = vec3 (0.5, 0.5, 0.5); // replace me later"
    , "    "
    , "    // specular intensity"
    , "    vec3 Is = vec3 (0.0, 0.0, 0.0); // replace me later"
    , "    "
    , "    // final colour"
    , "    fragmentColour = vec4 (Is + Id + Ia, 1.0);"
    , "}"
    ]

