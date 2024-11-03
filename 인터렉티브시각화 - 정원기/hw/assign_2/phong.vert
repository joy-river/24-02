#version 140
#extension GL_ARB_compatibility: enable

in vec4 vPosition;
in vec4 vNormal;
out vec4 normal;

uniform mat4 Mvp;
uniform mat4 N;

void main() 
{
   gl_Position = Mvp * vPosition;
   normal = N * vNormal;
}


