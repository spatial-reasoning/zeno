#version 130
attribute vec3 vertexPos;
attribute vec3 vertexNormal;
attribute vec3 vertexColor;
varying vec4 position;  // position of the vertex (and fragment) in camera space
varying vec3 normalDirection;  // surface normal vector in camera space
varying vec4 diffuseColor;  // surface normal vector in camera space
uniform mat4 cam;
uniform mat4 projcam;
 
void main()
{
  position = cam * vec4(vertexPos, 1);
  normalDirection = vec3(cam * vec4(vertexNormal, 0));
  diffuseColor = vec4(vertexColor, 1);
 
  gl_Position = projcam * vec4(vertexPos, 1);
}
