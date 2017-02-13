#version 130
in vec3 vertexPos;
uniform mat4 projcam;
out vec2 texCoord;

void main() {
  texCoord = vertexPos.xz * 0.25f;
  gl_Position = projcam * vec4(vertexPos, 1.0);
}
