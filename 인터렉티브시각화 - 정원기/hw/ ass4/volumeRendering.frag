#version 140
#extension GL_ARB_compatibility: enable

in vec3 pixelPosition; // 픽셀의 3D 위치

uniform vec3 eyePosition; // 카메라 위치
uniform vec3 up; // 카메라 위쪽 벡터
uniform vec3 objectMin; // 큐브 최소 좌표
uniform vec3 objectMax; // 큐브 최대 좌표
uniform sampler3D tex; // 3D 텍스처
uniform sampler1D transferFunction; // 트랜스퍼 함수

out vec4 fragColor; // 출력 색상

void main() {
    // 1. 카메라 방향 벡터 계산
    vec3 lookAt = vec3(eyePosition.x, eyePosition.y, 0.0);
    vec3 w = normalize(eyePosition - lookAt); // 뷰 방향
    vec3 u = normalize(cross(up, w));        // 오른쪽 벡터
    vec3 v = normalize(up);

    float d = dot(eyePosition - lookAt, w);

    vec3 rayDir = -d * w + pixelPosition.x * u + pixelPosition.y * v;

    // 2. Ray-Cube Intersection
    vec3 tMin = (objectMin - eyePosition) / max(abs(rayDir), vec3(1e-6));
    vec3 tMax = (objectMax - eyePosition) / max(abs(rayDir), vec3(1e-6));

    vec3 tEntry = min(tMin, tMax);
    vec3 tExit = max(tMin, tMax);

    float t0 = max(max(tEntry.x, tEntry.y), tEntry.z); // 진입점
    float t1 = min(min(tExit.x, tExit.y), tExit.z);    // 나가는 점

    if (t0 > t1 || t1 < 0.0) {
        discard; // 교차하지 않으면 버림
    }

    vec3 entryPoint = eyePosition + t0 * rayDir;
    vec3 exitPoint = eyePosition + t1 * rayDir;

    if (entryPoint == exitPoint) discard;

    
    vec3 voxelCoord = entryPoint;
    vec3 stepDir = (exitPoint - entryPoint) / length(exitPoint - entryPoint);

    float stepSize = length(exitPoint - entryPoint) / float(256); // 샘플링 스텝 크기

    float maxIntensity = 0.0f;
    vec4 accColor = vec4(0.0);
    vec4 transColor = vec4(0.0f);

    for (int i = 0; i < 256; i++) {
        float intensity = texture(tex, voxelCoord).r; // 3D 텍스처에서 샘플링
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
