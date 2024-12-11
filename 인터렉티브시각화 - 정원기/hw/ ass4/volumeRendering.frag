#version 140
#extension GL_ARB_compatibility: enable

in vec3 pixelPosition; // �ȼ��� 3D ��ġ

uniform vec3 eyePosition; // ī�޶� ��ġ
uniform vec3 up; // ī�޶� ���� ����
uniform vec3 objectMin; // ť�� �ּ� ��ǥ
uniform vec3 objectMax; // ť�� �ִ� ��ǥ
uniform sampler3D tex; // 3D �ؽ�ó
uniform sampler1D transferFunction; // Ʈ������ �Լ�

out vec4 fragColor; // ��� ����

void main() {
    // 1. ī�޶� ���� ���� ���
    vec3 lookAt = vec3(eyePosition.x, eyePosition.y, 0.0);
    vec3 w = normalize(eyePosition - lookAt); // �� ����
    vec3 u = normalize(cross(up, w));        // ������ ����
    vec3 v = normalize(up);

    float d = dot(eyePosition - lookAt, w);

    vec3 rayDir = -d * w + pixelPosition.x * u + pixelPosition.y * v;

    // 2. Ray-Cube Intersection
    vec3 tMin = (objectMin - eyePosition) / max(abs(rayDir), vec3(1e-6));
    vec3 tMax = (objectMax - eyePosition) / max(abs(rayDir), vec3(1e-6));

    vec3 tEntry = min(tMin, tMax);
    vec3 tExit = max(tMin, tMax);

    float t0 = max(max(tEntry.x, tEntry.y), tEntry.z); // ������
    float t1 = min(min(tExit.x, tExit.y), tExit.z);    // ������ ��

    if (t0 > t1 || t1 < 0.0) {
        discard; // �������� ������ ����
    }

    vec3 entryPoint = eyePosition + t0 * rayDir;
    vec3 exitPoint = eyePosition + t1 * rayDir;

    if (entryPoint == exitPoint) discard;

    
    vec3 voxelCoord = entryPoint;
    vec3 stepDir = (exitPoint - entryPoint) / length(exitPoint - entryPoint);

    float stepSize = length(exitPoint - entryPoint) / float(256); // ���ø� ���� ũ��

    float maxIntensity = 0.0f;
    vec4 accColor = vec4(0.0);
    vec4 transColor = vec4(0.0f);

    for (int i = 0; i < 256; i++) {
        float intensity = texture(tex, voxelCoord).r; // 3D �ؽ�ó���� ���ø�
        if (intensity > maxIntensity) {
            maxIntensity = intensity;
            accColor = texture(transferFunction, intensity);
        }

        // Advance sample position
        voxelCoord += stepDir * stepSize;
    }

   //fragColor = vec4(1.0, 0.0, 0.0, 0.0);
   fragColor = accColor;
}
