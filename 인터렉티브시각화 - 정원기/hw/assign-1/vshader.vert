#version 140
#extension GL_ARB_compatibility: enable

out vec4 color;

uniform mat4 Mv;
uniform mat4 Proj; 

void main() 
{
   gl_Position = Proj * Mv * gl_Vertex; 
   color = gl_Color;   
}


