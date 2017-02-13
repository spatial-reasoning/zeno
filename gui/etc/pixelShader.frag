#version 130
varying vec4 position;  // position of the vertex (and fragment) in camera space
varying vec3 normalDirection;  // surface normal vector in camera space
varying vec4 diffuseColor;  // surface normal vector in camera space

 
struct lightSource
{
  vec4 position;
  vec4 diffuse;
  vec4 specular;
  float constantAttenuation, linearAttenuation, quadraticAttenuation;
  float spotCutoff, spotExponent;
  vec3 spotDirection;
};
lightSource light0 = lightSource(
  vec4(0.0,  0.0,  0.0, 0.0),
  vec4(1.0,  1.0,  1.0, 1.0),
  vec4(1.0,  1.0,  1.0, 0.0),
  1.0, 0.0, 0.0,
  105.0, 1.0,
  // We have a right hand rule system. Left hand would be more intuitive.
  // We should transpose the transformation matrix and multiply it from the
  // right side.
  // x = right, y = up, z = back
  vec3(0.0, 0.0, -1.0)
);
vec4 scene_ambient = vec4(0.2, 0.2, 0.2, 1.0);
 
struct material
{
  vec4 ambient;
  vec4 diffuse;
  vec4 specular;
  float shininess;
};
material frontMaterial = material(
//  vec4(1.0, 1.0, 1.0, 1.0),
  diffuseColor,
  diffuseColor,
  vec4(1.0, 1.0, 1.0, 1.0),
  150.0
);
 
void main()
{
  vec3 normalDirection = normalize(normalDirection);
  vec3 viewDirection = normalize(vec3(vec4(0.0, 0.0, 0.0, 1.0) - position));
  vec3 positionToLightSource = vec3(light0.position - position);
  vec3 positionToLight = normalize(positionToLightSource);
  float distance = length(positionToLightSource);
  float attenuation = 1 / (  light0.constantAttenuation
                          + light0.linearAttenuation * distance
                          + light0.quadraticAttenuation * distance * distance);
 
  // point light or spotlight (or other kind of light) ?
  if (1.0 == light0.position.w) {
      // spotlight?
      if (light0.spotCutoff <= 90.0) {
          float clampedCosine = max(0.0, dot(-positionToLight, light0.spotDirection));
          // outside of spotlight cone?
          if (clampedCosine < cos(radians(light0.spotCutoff))) {
              attenuation = 0.0;
          } else {
              attenuation = attenuation
                          * pow(clampedCosine, light0.spotExponent);   
  }   }   }
 
  vec3 ambientLighting = vec3(scene_ambient) * vec3(frontMaterial.ambient);
 
  vec3 diffuseReflection = attenuation 
    * vec3(light0.diffuse) * vec3(frontMaterial.diffuse)
    * max(0.0, dot(normalDirection, positionToLight));
 
  vec3 specularReflection;
  if (dot(normalDirection, positionToLight) < 0.0) // light source on the wrong side?
    {
      specularReflection = vec3(0.0, 0.0, 0.0); // no specular reflection
    }
  else // light source on the right side
    {
      specularReflection = attenuation
          * vec3(light0.specular) * vec3(frontMaterial.specular) 
          * pow( max( 0.0
                    , dot( reflect(- positionToLight, normalDirection)
                         , viewDirection) )
               , frontMaterial.shininess)
          / pow(distance, 0.5);
    }
 
  gl_FragColor = vec4(ambientLighting + diffuseReflection + specularReflection, 1.0);
}

