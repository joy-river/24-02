#version 140
#extension GL_ARB_compatibility : enable

in vec3 worldPos;
out vec4 fragColor;

void main() {
    fragColor = vec4(worldPos, 1.0);
}