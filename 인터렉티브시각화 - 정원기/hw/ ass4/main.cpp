/*
 * Skeleton code for COSE436 Fall 2024
 *
 * Won-Ki Jeong, wkjeong@korea.ac.kr
 *
 */

#include <stdio.h>
#include <GL/glew.h>
#include <GL/glut.h>

#include <iostream>
#include <assert.h>
#include "textfile.h"
#include "tfeditor.h"
#include "Angel.h"


#define FILE_NAME "../data/CThead_512_512_452.raw"
#define W 512
#define H 512
#define D 452


//#define FILE_NAME "../data/tooth_100_90_160.raw"
//#define W 100
//#define H 90
//#define D 160

//#define FILE_NAME "../data/bonsai_256_256_256.raw"
//#define W 256
//#define H 256
//#define D 256

//#define FILE_NAME "../data/Bucky_32_32_32.raw"
//#define W 32
//#define H 32
//#define D 32

//#define FILE_NAME "../data/lung_256_256_128.raw"
//#define W 256
//#define H 256
//#define D 128



float vertices[] = {
	// Front face
	0.0f, 0.0f, 1.0f,
	1.0f, 0.0f, 1.0f,
	1.0f, 1.0f, 1.0f,
	0.0f, 1.0f, 1.0f,

	// Back face
	0.0f, 0.0f, 0.0f,
	1.0f, 0.0f, 0.0f,
	1.0f, 1.0f, 0.0f,
	0.0f, 1.0f, 0.0f,

	// Left face
	0.0f, 0.0f, 0.0f,
	0.0f, 0.0f, 1.0f,
	0.0f, 1.0f, 1.0f,
	0.0f, 1.0f, 0.0f,

	// Right face
	1.0f, 0.0f, 0.0f,
	1.0f, 0.0f, 1.0f,
	1.0f, 1.0f, 1.0f,
	1.0f, 1.0f, 0.0f,

	// Top face
	0.0f, 1.0f, 0.0f,
	1.0f, 1.0f, 0.0f,
	1.0f, 1.0f, 1.0f,
	0.0f, 1.0f, 1.0f,

	// Bottom face
	0.0f, 0.0f, 0.0f,
	1.0f, 0.0f, 0.0f,
	1.0f, 0.0f, 1.0f,
	0.0f, 0.0f, 1.0f
};

// Glut windows
int volumeRenderingWindow;
int transferFunctionWindow;

// Shader programs
GLuint p;

// Texture object
GLuint objectTex;
GLuint transferTex;

// Trackball things
float lastPos[3], curPos[3];
float zoom = 40.0f;
float transX = 0.0f, transY = 0.0f;

float startX = 0.0f, startY = 0.0f;

bool mouseLeft = false, mouseRight = false;
int mode = 1;
float isoValue = 0.55f;

// shader things
float K[3] = { 0.8, 0.8, 0.8 }; // kd ka ks for Phong
float A = 0.5; // Alpha For Specular
vec3 lightPos = vec3(0.0f, 1.0f, 0.0f);
vec3 lightColor = vec3(0.0f, 0.3f, 0.5f);
float alphaPow = 2.0f;

// projection Matrix
mat4 projMat;
// model Matrix
mat4 mMat;
// camera Matrix
mat4 vMat;
// Rotation Matrix
mat4 rMat = identity();

GLuint VBO, IBO;


// Buffer init
void initBuffers() {
	unsigned int indices[] = {
	0, 1, 2,  2, 3, 0,   // Front face
	4, 5, 6,  6, 7, 4,   // Back face
	8, 9, 10, 10, 11, 8, // Left face
	12, 13, 14, 14, 15, 12, // Right face
	16, 17, 18, 18, 19, 16, // Top face
	20, 21, 22, 22, 23, 20  // Bottom face
	};

	glGenBuffers(1, &VBO);
	glBindBuffer(GL_ARRAY_BUFFER, VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

	glGenBuffers(1, &IBO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IBO);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

}
// Camera Matrix
void setCam(void) {
	vMat = LookAt(vec4(transX, transY, zoom, 0.0),  // position
		vec4(transX, transY, 0.0, 1.0),  // lookat
		vec4(0.0, 1.0, 0.0, 0.0)); // upvector
}
// perspective Projection
void projPerspective() {
	float ratio = (float)glutGet(GLUT_WINDOW_WIDTH) / glutGet(GLUT_WINDOW_HEIGHT);
	projMat = Perspective(45.0, ratio, 0.01, 500.0); // fov, aspect, near, far
}

// Orthogonal Projection
void projOrtho(void) {
	std::cout << "Orthogonal Projection" << std::endl;
	projMat = Ortho(-1.0, 1.0, -1.0, 1.0, 0.1, 100.0); // left, right, bottom, top, near, far
}

void trackball_ptov(int x, int y, int width, int height, float v[3]) {
	float d, a;

	// Normalize x, y
	v[0] = (2.0f * x - width) / width;
	v[1] = (height - 2.0f * y) / height;

	// Calculate z
	d = sqrt(v[0] * v[0] + v[1] * v[1]);
	v[2] = (d < 1.0f) ? sqrt(1.0f - d * d) : 0.0f;

	// Normalize
	a = 1.0f / sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
	v[0] *= a;
	v[1] *= a;
	v[2] *= a;
}

// Rotate func for trackball
void Rotate(float angle, float x, float y, float z) {
	mat4 result = mat4(1.0f);

	float radians = angle * M_PI / 180.0f;
	float c = cos(radians);
	float s = sin(radians);
	float oneMinusC = 1.0f - c;

	float length = sqrt(x * x + y * y + z * z);
	if (length > 0.0f) {
		x /= length;
		y /= length;
		z /= length;
	}

	result[0][0] = c + x * x * oneMinusC;
	result[0][1] = x * y * oneMinusC - z * s;
	result[0][2] = x * z * oneMinusC + y * s;

	result[1][0] = y * x * oneMinusC + z * s;
	result[1][1] = c + y * y * oneMinusC;
	result[1][2] = y * z * oneMinusC - x * s;

	result[2][0] = z * x * oneMinusC - y * s;
	result[2][1] = z * y * oneMinusC + x * s;
	result[2][2] = c + z * z * oneMinusC;

	
	rMat = result * rMat;
}

void mouse(int button, int state, int x, int y) {
	if (button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
		mouseLeft = true;
		trackball_ptov(x, y, glutGet(GLUT_WINDOW_WIDTH), glutGet(GLUT_WINDOW_HEIGHT), lastPos);
	}
	else if (button == GLUT_LEFT_BUTTON && state == GLUT_UP) {
		mouseLeft = false;
	}

	if (button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN) {
		mouseRight = true;
	}
	else if (button == GLUT_RIGHT_BUTTON && state == GLUT_UP) {
		mouseRight = false;
	}

	if (button == GLUT_MIDDLE_BUTTON && state == GLUT_DOWN) {
		zoom = 40.0f;
		transX = 0.0f;
		transY = 0.0f;
		rMat = identity();

		setCam();
		glutPostRedisplay();
	}
}

void motion(int x, int y) {
	if (mouseLeft) {
		// 좌클릭으로 회전 구현
		trackball_ptov(x, y, glutGet(GLUT_WINDOW_WIDTH), glutGet(GLUT_WINDOW_HEIGHT), curPos);

		float dx = curPos[0] - lastPos[0];
		float dy = curPos[1] - lastPos[1];
		float dz = curPos[2] - lastPos[2];

		if (dx || dy || dz) {
			vec3 axis = vec3(
				lastPos[1] * curPos[2] - lastPos[2] * curPos[1],
				lastPos[2] * curPos[0] - lastPos[0] * curPos[2],
				lastPos[0] * curPos[1] - lastPos[1] * curPos[0]
			);

			float angle = 90.0f * sqrt(dx * dx + dy * dy + dz * dz);
			Rotate(angle, axis[0], axis[1], axis[2]);

			lastPos[0] = curPos[0];
			lastPos[1] = curPos[1];
			lastPos[2] = curPos[2];

			glutPostRedisplay();
		}
	}

	if (mouseRight) {
		// 우클릭으로 줌인/줌아웃 구현
		float deltaY = (startY - y) * 0.05f; // y 축 기준으로 변화량 계산
		zoom += deltaY;

		if (zoom < 5.0f) zoom = 5.0f;      // 최소 줌인 한계
		if (zoom > 100.0f) zoom = 100.0f; // 최대 줌아웃 한계

		startY = y; // 현재 y 값을 저장
		setCam();
		glutPostRedisplay();
	}
}


//
// Loading volume file, create 3D texture and its histogram
//
void load3Dfile(char *filename,int w,int h,int d) {

	// loading volume data
	FILE *f = fopen(filename, "rb");
	unsigned char *data = new unsigned char[w*h*d];
	fread(data, 1, w*h*d, f);
	fclose(f);

	// generate 3D texture
	glGenTextures(1, &objectTex);
	glBindTexture(GL_TEXTURE_3D, objectTex);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glTexImage3D(GL_TEXTURE_3D, 0, GL_RED, w, h, d, 0, GL_RED, GL_UNSIGNED_BYTE, data);

	// create histogram
	for (int i = 0; i<256; i++) {
		histogram[i] = 0;
	}
	for (int i = 0; i < w*h*d; i++) {
		histogram[data[i]]++;
	}
	for (int i = 0; i<256; i++) {
		histogram[i] /= w*h*d;
	}

	delete[]data;
}


void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window of zero width).
	if(h == 0) h = 1;
	float ratio = 1.0f * (float) w / (float)h;

	// Set the viewport to be the entire window
    glViewport(0, 0, w, h);
}


void keyboard(unsigned char key, int x, int y)
{
	if (key == '1') {
		std::cout << "MIP mode" << std::endl;
		mode = 1;
	}
	else if (key == '2') {
		std::cout << "Iso-surface mode" << std::endl;
		mode = 2;
	}
	else if (key == '3') {
		std::cout << "Alpha Compositing mode" << std::endl;
		mode = 3;
	}

	glutPostRedisplay();
}

void shaderUnif(void) {
	glUseProgram(p);

	// 객체 변환 행렬: rMat는 객체 회전, Translate/Scale로 객체 범위 조정
	mat4 modelMat =  Translate(-5.0f, -5.0f, 0.0f) * Scale(10.0f, 10.0f, 10.0f);
	mat4 mvpMat = projMat * vMat * rMat * modelMat;

	glUniformMatrix4fv(glGetUniformLocation(p, "Mvp"), 1, GL_TRUE, mvpMat);

	// 모델 행렬 역행렬 계산 및 전달
	mat4 invModelMat = Scale(0.1f, 0.1f, 0.1f) * Translate(5.0f, 5.0f, 0.0f) * transpose(rMat);
	glUniformMatrix4fv(glGetUniformLocation(p, "invMat"), 1, GL_TRUE, invModelMat);

	// Viewer parameters
	vec3 camPos = vec3(transX, transY, zoom);
	vec3 upVec = vec3(0.0, 1.0, 0.0);
	glUniform3fv(glGetUniformLocation(p, "eyePosition"), 1, &camPos[0]);
	glUniform3fv(glGetUniformLocation(p, "up"), 1, &upVec[0]);
	glUniform3fv(glGetUniformLocation(p, "objectMin"), 1, vec3(0.0f, 0.0f, 0.0f));
	glUniform3fv(glGetUniformLocation(p, "objectMax"), 1, vec3(1.0f, 1.0f, 1.0f));
	glUniform1f(glGetUniformLocation(p, "ratio"), (float)glutGet(GLUT_WINDOW_WIDTH) / (float)glutGet(GLUT_WINDOW_HEIGHT));


	// for rendering
	glUniform1i(glGetUniformLocation(p, "renderMode"), mode);

	// Iso + phong
	glUniform1f(glGetUniformLocation(p, "isoValue"), isoValue);
	glUniform1f(glGetUniformLocation(p, "Kd"), K[0]);
	glUniform1f(glGetUniformLocation(p, "Ka"), K[1]);
	glUniform1f(glGetUniformLocation(p, "Ks"), K[2]);
	glUniform1f(glGetUniformLocation(p, "specAlpha"), A);
	glUniform3fv(glGetUniformLocation(p, "lightPos"), 1, lightPos);
	glUniform3fv(glGetUniformLocation(p, "lightColor"), 1, lightColor);

	// Alpha compositing
	glUniform1f(glGetUniformLocation(p, "alphaPow"), alphaPow);

	// Volume texture
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_3D, objectTex);
	glUniform1i(glGetUniformLocation(p, "tex"), 0);

	// Transfer function texture
	glActiveTexture(GL_TEXTURE1);
	glBindTexture(GL_TEXTURE_1D, transferTex);
	glUniform1i(glGetUniformLocation(p, "transferFunction"), 1);

}

void renderVolume() {
	shaderUnif();

	glBindBuffer(GL_ARRAY_BUFFER, VBO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IBO);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), (void*)0);


	glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);


	// Debug transfer function values
	std::cout << "R,G,B,A: " << transferFunction[0] << "," << transferFunction[1] << "," << transferFunction[2] << "," << transferFunction[3] << std::endl;

	glDisableVertexAttribArray(0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
}

void renderScene(void) 
{
	glClearColor(0, 0, 0, 0);

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	renderVolume();


	glutSwapBuffers();
}

void inittransferTex() {
	glGenTextures(1, &transferTex);
	glBindTexture(GL_TEXTURE_1D, transferTex);

	glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);

	glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, 256, 0, GL_RGBA, GL_FLOAT, transferFunction);
}

void idle()
{
	if (transferFunctionChanged) {
		glutSetWindow(volumeRenderingWindow);
		inittransferTex();
		transferFunctionChanged = false;
		glutPostRedisplay();
	}
}



void init() 
{
	load3Dfile(FILE_NAME, W, H, D);
	glUseProgram(p);
		
	for (int i = 0; i < 256; i++) {
		transferFunction[i * 4 + 0] = float(i) / 255.0;
		transferFunction[i * 4 + 1] = float(i) / 255.0;
		transferFunction[i * 4 + 2] = float(i) / 255.0;
		transferFunction[i * 4 + 3] = float(i) / 255.0;
	}

	inittransferTex();
	setCam();
	projPerspective();
	initBuffers();


}


int main(int argc, char **argv) 
{
	glutInit(&argc, argv);

	//
	// 1. Transfer Function Editor Window
	//
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 700);
	glutInitWindowSize(600, 300);
	transferFunctionWindow = glutCreateWindow("Transfer Function");

	// register callbacks
	glutDisplayFunc(renderScene_transferFunction);
	glutReshapeFunc(changeSize_transferFunction);

	glutMouseFunc(mouseClick_transferFunction);
	glutMotionFunc(mouseMove_transferFunction);
	glutIdleFunc(idle);

	init_transferFunction();

	//
	// 2. Main Volume Rendering Window
	//
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(600,600);
	volumeRenderingWindow = glutCreateWindow("Volume Rendering");

	// register callbacks
	glutDisplayFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutKeyboardFunc(keyboard);

	glutMouseFunc(mouse);
	glutMotionFunc(motion);

	glutIdleFunc(idle);

	glEnable(GL_DEPTH_TEST);

	glewInit();
	if (glewIsSupported("GL_VERSION_3_3"))
		printf("Ready for OpenGL 3.3\n");
	else {
		printf("OpenGL 3.3 is not supported\n");
		exit(1);
	}

	// Create shader program
	p = createGLSLProgram( "../volumeRendering.vert", NULL, "../volumeRendering.frag" );

	init();

	// enter GLUT event processing cycle
	glutMainLoop();

	return 1;
}