#version 140
#extension GL_ARB_compatibility: enable

out vec3 pixelPosition;
uniform mat4 Mvp;

void main() {
    gl_Position = Mvp * gl_Vertex;
    // NDC º¯È¯
    pixelPosition = (gl_Position.xyz / gl_Position.w);
}