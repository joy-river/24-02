#version 140
#extension GL_ARB_compatibility: enable


out vec3 pixelPosition;
uniform mat4 Mvp;

void main(){
    gl_Position = Mvp * gl_Vertex; 
    pixelPosition=vec3(gl_Vertex);
   
}