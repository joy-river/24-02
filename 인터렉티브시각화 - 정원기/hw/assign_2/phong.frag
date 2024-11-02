#version 140
#extension GL_ARB_compatibility: enable

uniform vec4 lightpos;
uniform vec4 Kd; // Material property
uniform vec4 Ld; // Light property
in vec4 normal;

void main() 
{ 
    vec4 n = normalize(normal);
    vec4 l = normalize(lightpos);
    vec4 diffuse = max(dot(l, n), 0) * Kd * Ld;
    gl_FragColor = diffuse;
} 
