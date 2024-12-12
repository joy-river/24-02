#version 140
#extension GL_ARB_compatibility: enable

in vec3 pixelPosition;

uniform vec3 eyePosition;
uniform vec3 up;
uniform vec3 objectMin;
uniform vec3 objectMax;
uniform sampler3D tex;
uniform sampler1D transferFunction;

uniform float ratio;
uniform mat4 invMat;
uniform int renderMode;   // 1: MIP, 2: iso-surface, 3: alpha compositing
uniform float isoValue;

// Iso-surface용 파라미터 (예시)
uniform vec3 lightPos;
uniform vec3 lightColor;
uniform float Kd, Ka, Ks; 
uniform float specAlpha;

// Alpha compositing용 파라미터
uniform float alphaPow; // alpha 조정을 위한 pow 값 (예: 2.0)

// Output
out vec4 fragColor;

// Gradient 계산 함수(iso-surface용)
vec3 computeGradient(vec3 pos) {
    float delta = 1.0/512.0; // 볼륨 해상도에 따라 조정
    vec3 dX = vec3(delta,0,0);
    vec3 dY = vec3(0,delta,0);
    vec3 dZ = vec3(0,0,delta);

    vec3 normCoordP, normCoordM;
    float IxP, IxM, IyP, IyM, IzP, IzM;

    normCoordP = (pos + dX - objectMin)/(objectMax - objectMin);
    normCoordM = (pos - dX - objectMin)/(objectMax - objectMin);
    IxP = texture(tex, normCoordP).r;
    IxM = texture(tex, normCoordM).r;

    normCoordP = (pos + dY - objectMin)/(objectMax - objectMin);
    normCoordM = (pos - dY - objectMin)/(objectMax - objectMin);
    IyP = texture(tex, normCoordP).r;
    IyM = texture(tex, normCoordM).r;

    normCoordP = (pos + dZ - objectMin)/(objectMax - objectMin);
    normCoordM = (pos - dZ - objectMin)/(objectMax - objectMin);
    IzP = texture(tex, normCoordP).r;
    IzM = texture(tex, normCoordM).r;

    vec3 grad = vec3((IxP - IxM)/(2.0*delta),
                     (IyP - IyM)/(2.0*delta),
                     (IzP - IzM)/(2.0*delta));
    return grad;
}

void main() {
    // Camera
    vec3 lookAt = vec3(eyePosition.x, eyePosition.y, 0.0);
    vec3 w = normalize(eyePosition - lookAt);
    vec3 u = normalize(cross(up, w));
    vec3 v = cross(w, u);

    float fov = 45.0;
    float near = 0.01;

    float fovY = radians(fov);
    float tanFovY = tan(fovY * 0.5);
    float near_height = 2.0 * tanFovY * near; 
    float near_width = near_height * ratio;

    vec3 center = eyePosition - w * near;

    vec3 pixelPosOnPlane = center
        + pixelPosition.x * (near_width * 0.5) * u
        + pixelPosition.y * (near_height * 0.5) * v;

   // Ray intersection
    vec3 rayDir = normalize(pixelPosOnPlane - eyePosition);

    vec4 ep_obj = invMat * vec4(eyePosition, 1.0);
    vec4 rd_obj = invMat * vec4(rayDir, 0.0);
    vec3 eyePos_obj = ep_obj.xyz / ep_obj.w;
    vec3 rayDir_obj = normalize(rd_obj.xyz);

    vec3 invDir = 1.0/(rayDir_obj+1e-6); 
    vec3 tMin = (objectMin - eyePos_obj)*invDir;
    vec3 tMax = (objectMax - eyePos_obj)*invDir;
    vec3 tEntry = min(tMin,tMax);
    vec3 tExit = max(tMin,tMax);

    float t0 = max(max(tEntry.x, tEntry.y), tEntry.z);
    float t1 = min(min(tExit.x, tExit.y), tExit.z);

    if (t0 > t1) {
        discard;
        return;
    }

    vec3 entryPoint = eyePos_obj + t0*rayDir_obj;
    vec3 exitPoint = eyePos_obj + t1*rayDir_obj;

    vec3 dir = normalize(exitPoint - entryPoint);
    float lengthRay = length(exitPoint - entryPoint);

    // Ray marching
    int numSteps = 512;
    float stepSize = lengthRay / float(numSteps);
    vec3 voxelCoord = entryPoint;

    float maxIntensity = 0.0;
    vec4 accColor = vec4(0.0);
    float accAlpha = 0.0;

    float prevIntensity = 0.0;
    vec3 prevCoord;

    for (int i = 0; i < numSteps; i++) {
        vec3 normalizedCoord = (voxelCoord - objectMin) / (objectMax - objectMin);

        if (any(lessThan(normalizedCoord, vec3(0.0))) || any(greaterThan(normalizedCoord, vec3(1.0)))) {
            break;
        }

        float intensity = texture(tex, normalizedCoord).r;

        // Max Intensity
        if (renderMode == 1) {
            maxIntensity = max(maxIntensity, intensity);
        } 
        else if (renderMode == 2) {
            // iso-surface
            if (i == 0) {
                prevIntensity = intensity;
                prevCoord = voxelCoord;
            } 
            else {
                float val_prev = prevIntensity - isoValue;
                float val_cur  = intensity - isoValue;
                if (val_prev * val_cur < 0.0) {
                    // zero-crossing
                    float t = val_prev / (val_prev - val_cur);
                    vec3 isoPos = prevCoord + dir * stepSize * t;

                    // Gradient 계산
                    vec3 grad = computeGradient(isoPos);
                    vec3 N = normalize(grad);

                    // 간단한 퐁 조명 예제 (월드 좌표 가정)
                    vec3 L = normalize(lightPos - isoPos);
                    vec3 V = normalize(eyePosition - isoPos);
                    vec3 R = reflect(-L, N);

                    vec3 diff = max(dot(N,L),0.0) * Kd * lightColor;
                    vec3 ambient = Ka * lightColor;
                    vec3 spec = Ks * pow(max(dot(R,V),0.0), specAlpha) * lightColor;

                    vec3 finalColor = diff + ambient + spec;
                    fragColor = vec4(finalColor, 1.0);
                    return;
                }

                prevIntensity = intensity;
                prevCoord = voxelCoord;
            }

        } else if (renderMode == 3) {
            // alpha compositing
            vec4 sampleColor = texture(transferFunction, intensity);
            // 알파값 재조정
            sampleColor.a = pow(sampleColor.a, alphaPow);

            // front-to-back 합성
            float alphaFactor = (1.0 - accAlpha) * sampleColor.a;
            accColor.rgb += alphaFactor * sampleColor.rgb;
            accAlpha += alphaFactor;

            // early termination
            if (accAlpha > 0.90) {
                break;
            }
        }

        voxelCoord += dir * stepSize;
    }

    // 모드별 결과 출력
    if (renderMode == 1) {
        // MIP
        fragColor = vec4(maxIntensity, maxIntensity, maxIntensity, 1.0);
    } else if (renderMode == 2) {
        // No iso Color
        fragColor = vec4(0.0);
        discard;
    } else if (renderMode == 3) {
        fragColor = vec4(accColor.rgb, accAlpha);
    }
}
