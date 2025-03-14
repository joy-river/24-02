/*
 * Skeleton code for COSE436 Fall 2024 Assignment 2
 *
 * Won-Ki Jeong, wkjeong@korea.ac.kr
 */

#include <stdio.h>
#include <GL/glew.h>
#include <GL/glut.h>

#include <iostream>
#include <assert.h>
#include "textfile.h"
#include "Angel.h"


 // Shader programs
GLuint p[3];

// uniform locations
GLuint p0_Mvp;
GLuint p1_Mvp;
GLuint p2_Mvp;
GLuint N;

// Buffer Object
GLuint VBO[3], IBO[3];
vec4* vPosition[3] = { nullptr, nullptr, nullptr };
vec4* vNormal[3] = { nullptr, nullptr, nullptr };
unsigned int* fIndices[3] = { nullptr, nullptr, nullptr };

int vNum[3] = { 0, 0, 0 };
int fNum[3] = { 0, 0, 0 };
int eNum[3] = { 0, 0, 0 };

// projection Matrix
mat4 projMat;
// model Matrix
mat4 mMat;
// camera Matrix
mat4 vMat;

// select model
int model = 0;
// Trans = False, Rotate = True(default = false)
bool mode = false;
// Axis for Trans or Rotate(default = x)
char trans_axis = 'x';

// Read OFF file
void readFiles() {
	const char* files[3] = { "../mesh-data/bunny.off", "../mesh-data/dragon-full.off", "../mesh-data/fandisk.off" };
	FILE* f;
	char header[4];

	for (int i = 0; i < 3; i++) {
		f = fopen(files[i], "r");

		if (!f) {
			std::cout << "FIle error" << std::endl;
		}

		fscanf(f, "%3s", header);

		if (std::string(header) != "OFF")
			return;

		fscanf(f, "%d %d %d", &vNum[i], &fNum[i], &eNum[i]);


		vPosition[i] = new vec4[vNum[i]];
		vNormal[i] = new vec4[vNum[i]];

		// read vertor position
		for (int j = 0; j < vNum[i]; j++) {
			float x, y, z;

			if (fscanf(f, "%f %f %f", &x, &y, &z) == 3) {
				vPosition[i][j] = vec4(x, y, z, 1.0f);
				vNormal[i][j] = vec4(0.0f, 0.0f, 0.0f, 0.0f);
			}
		}

		fIndices[i] = new unsigned int[fNum[i] * 3];	

		// reading indices
		for (int j = 0; j < fNum[i]; j++) {
			unsigned int n;
			unsigned int index[3];

			fscanf(f, "%d", &n);

			for (int s = 0; s < n; s++) {
				fscanf(f, "%d", &index[s]);
				fIndices[i][j * 3 + s] = index[s];
			}

			vec4 v0 = vPosition[i][index[0]];
			vec4 v1 = vPosition[i][index[1]];
			vec4 v2 = vPosition[i][index[2]];

			// per face normal
			vec4 normal = normalize(cross(v1 - v0, v2 - v0));

			vNormal[i][index[0]] += normal;
			vNormal[i][index[1]] += normal;
			vNormal[i][index[2]] += normal;
		}
	
		// per vertex normal
		for (int j = 0; j < vNum[i]; j++)
			vNormal[i][j] = normalize(vNormal[i][j]);

		fclose(f);
	}

}

// Buffer init
void initBuffers() {
	for (int i = 0; i < 3; i++) {
		glGenBuffers(1, &VBO[i]);
		glBindBuffer(GL_ARRAY_BUFFER, VBO[i]);
		glBufferData(GL_ARRAY_BUFFER, vNum[i] * sizeof(vec4), vPosition[i], GL_STATIC_DRAW);

		glGenBuffers(1, &IBO[i]);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IBO[i]);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, fNum[i] * 3 * sizeof(unsigned int), fIndices[i], GL_STATIC_DRAW);
	}
}


void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window of zero width).
	if(h == 0) h = 1;

	float ratio = 1.0f * (float) w / (float)h;

	// Set the viewport to be the entire window
	glViewport(0, 0, w, h);
		
}

// perspective Projection
void projPerspective(void) {
	std::cout << "Perspective Projection" << std::endl;
	projMat = Perspective(45.0, 1.0, 0.01, 100.0); // fov, aspect, near, far
}
// Orthogonal Projection
void projOrtho(void) {
	std::cout << "Orthogonal Projection" << std::endl;
	projMat = Ortho(-1.0, 1.0, -1.0, 1.0, 0.1, 100.0); // left, right, bottom, top, near, far
}

// Camera Matrix
void setCam(void) {
	vMat = LookAt(vec4(0.5, 0.5, 0.5, 1.0),  // position
		vec4(0.0, 0.05, 0.0, 1.0),  // lookat
		vec4(0.0, 1.0, 0.0, 0.0)); // upvector
}


void rotateModel(unsigned char key) {
	float angle = (key == GLUT_KEY_LEFT) ? -5.0f : 5.0f;

	// rotate Matrix
	mat4 rMat;

	// object's center
	vec4 position = vec4(mMat[0][3], mMat[1][3], mMat[2][3], 1.0);

	// translate Matrix
	mat4 tMat = Translate(-position.x, -position.y, -position.z);

	// inverse translate Matrix
	mat4 invtMat = Translate(position.x, position.y, position.z);

	// 선택된 축에 따른 회전 행렬 생성
	if (trans_axis == 'x')
		rMat = RotateX(angle);
	else if (trans_axis == 'y')
		rMat = RotateY(angle);
	else
		rMat = RotateZ(angle);

	// t^-1 * r * t 
	mMat = invtMat * rMat * tMat * mMat;

	glutPostRedisplay();
}

void transModel(unsigned char key) {
	float trans = (key == GLUT_KEY_LEFT) ? -0.5f : 0.5f;

	// translate Matrix
	mat4 tMat;

	if (trans_axis == 'x')
		tMat = Translate(trans, 0, 0);
	else if (trans_axis == 'y')
		tMat = Translate(0, trans, 0);
	else
		tMat = Translate(0, 0, trans);

	mMat = tMat * mMat;

	glutPostRedisplay();

}

void keyboard(unsigned char key, int x, int y)
{

	// ToDo
	if (key == 'p') {
		// perspective
		projPerspective();
	}
	if (key == 'o') {
		// orth
		projOrtho();
	}
	if (key == 'm') {
		// switch model
		model = (model + 1) % 3;
	}
	if (key == 't') {
		// set translate mode
		mode = false;
		std::cout << "Translate Mode Enabled" << std::endl;
	}
	if (key == 'r') {
		// set rotate mode
		mode = true;
		std::cout << "Rotate Mode Enabled" << std::endl;
	}

	if (key == 'x' || key == 'y' || key == 'z') {
		// do translate by x, y, z
		std::cout << "Translate Axis is " << key << std::endl;
		trans_axis = key;
	}

	if (key == 27) {
		// esc
		std::cout << "GoodBye!" << std::endl;
		exit(0);
	}
	glutPostRedisplay();
}

void arrowkeys(int key, int x, int y) {
	if (key == GLUT_KEY_LEFT || key == GLUT_KEY_RIGHT) {
		// rotate
		if (mode)
			rotateModel(key);
		else
			transModel(key);
	}
	glutPostRedisplay();
}

void renderScene(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	setCam();

	// Compute and send MVP matrix to shader
	mat4 mvpMat = projMat * vMat * mMat;
	mat3 nMat = Normal(vMat * mMat);
	glUseProgram(p[0]);
	glUniformMatrix4fv(p0_Mvp, 1, GL_TRUE, mvpMat);
	glUniformMatrix3fv(N, 1, GL_TRUE, nMat);


	// Bind the VBO and IBO for the current model
	glBindBuffer(GL_ARRAY_BUFFER, VBO[model]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IBO[model]);

	glEnableClientState(GL_VERTEX_ARRAY);
	glVertexPointer(4, GL_FLOAT, sizeof(vec4), 0);

	glDrawElements(GL_TRIANGLES, fNum[model] * 3, GL_UNSIGNED_INT, 0);

	glDisableClientState(GL_VERTEX_ARRAY);

	glutSwapBuffers();
}



void idle()
{
	glutPostRedisplay();
}


void init()
{
	glEnable(GL_DEPTH_TEST);
	glClearColor(1.0, 1.0, 1.0, 1.0); 
	
	// Create shader program
	p[0] = createGLSLProgram( "../phong.vert", NULL, "../phong.frag" ); // Phong
	p[1] = createGLSLProgram( "../silhouette.vert", NULL, "../silhouette.frag" ); // Silhouette
	p[2] = createGLSLProgram( "../toon.vert", NULL, "../toon.frag" ); // Cartoon

	// vertex shader location
	p0_Mvp = glGetUniformLocation(p[0], "Mvp");
	p1_Mvp = glGetUniformLocation(p[1], "Mvp");
	p2_Mvp = glGetUniformLocation(p[2], "Mvp");
	N = glGetUniformLocation(p[0], "N");


	glEnable(GL_DEPTH_TEST);
	glClearColor(1.0, 1.0, 1.0, 1.0);
}

int main(int argc, char **argv) {

	// init GLUT and create Window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(600,600);
	glutCreateWindow("COSE436 - Assignment 2");

	// register callbacks
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutKeyboardFunc(keyboard);
	glutIdleFunc(idle);
	// callback for arrowkeys
	glutSpecialFunc(arrowkeys);

	glewInit();
	if (glewIsSupported("GL_VERSION_3_3"))
		printf("Ready for OpenGL 3.3\n");
	else {
		printf("OpenGL 3.3 is not supported\n");
		exit(1);
	}

	init();
	// buffer init
	readFiles();
	initBuffers();
	projPerspective();

	// enter GLUT event processing cycle
	glutMainLoop();

	return 1;
}