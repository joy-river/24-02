#version 140
#extension GL_ARB_compatibility : enable

in vec3 inPosition;

uniform mat4 MVP;
uniform mat4 ModelMat;

out vec3 worldPos;

void main() {
    vec4 worldPos4 = ModelMat * vec4(inPosition, 1.0);
    worldPos = worldPos4.xyz;

    gl_Position = MVP * vec4(inPosition, 1.0);
}