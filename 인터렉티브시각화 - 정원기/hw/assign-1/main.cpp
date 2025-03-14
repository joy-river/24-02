	/*
	 * Skeleton code for COSE436 Fall 2024 Assignment 1
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
	#include "mat.h"


	// Shader programs
	GLuint p;
	// uniform locations
	GLuint Mv;
	GLuint Proj;

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
		projMat = Perspective(45.0, 1.0, 0.1, 100.0); // fov, aspect, near, far
	}
	// Orthogonal Projection
	void projOrtho(void) {
		std::cout << "Orthogonal Projection" << std::endl;
		projMat = Ortho(-1.0, 1.0, -1.0, 1.0, 0.1, 100.0); // left, right, bottom, top, near, far
	}

	// Camera Matrix
	void setCam(void) {
		vMat = LookAt(vec4(2.0, 2.0, 2.0, 1.0),  // position
					  vec4(0.0, 0.0, 0.0, 1.0),  // lookat
					  vec4(0.0, 1.0, 0.0, 0.0)); // upvector
	}

	void drawAxis(void) {
		glBegin(GL_LINES);
			glColor3f(1.0, 0.0, 0.0);
			glVertex3f(0.0, 0.0, 0.0);
			glVertex3f(10.0, 0.0, 0.0);

			glColor3f(0.0, 0.0, 1.0);
			glVertex3f(0.0, 0.0, 0.0);
			glVertex3f(0.0, 10.0, 0.0);

			glColor3f(0.0, 1.0, 0.0);
			glVertex3f(0.0, 0.0, 0.0);
			glVertex3f(0.0, 0.0, 10.0);
		glEnd();
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
		if(key == 'p') {
			// perspective
			projPerspective();
		}
		if (key == 'o') {
			// orth
			projOrtho();
		}
		if (key == 'm') {
			// switch model
			model = (model + 1) % 6;
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

		// ToDo
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		// set CamMatrix
		setCam();

		// draw axis on origin
		glUniformMatrix4fv(Mv, 1, GL_TRUE, vMat);
		drawAxis();  // 좌표축 그리기

		// send Matrix to vshader

		mat4 mvMat = vMat * mMat;
		glUniformMatrix4fv(Mv, 1, GL_TRUE, mvMat);
		glUniformMatrix4fv(Proj, 1, GL_TRUE, projMat);



		glColor3f(1, 0, 0);


		switch (model) {
		case 0 :
			glutWireTeapot(0.5);
			break;
		case 1 :
			glutWireCube(0.5);
			break;
		case 2 :
			glutWireSphere(0.5, 20, 20);
			break;
		case 3 :
			glutWireCone(0.5, 1.0, 20, 20);
			break;
		case 4 :
			glutWireTorus(0.5, 1.0, 20, 20);
			break;
		case 5 :
			glutWireIcosahedron();
			break;
		}


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
		p = createGLSLProgram("../vshader.vert", NULL, "../fshader.frag");

		// vertex shader location
		Mv = glGetUniformLocation(p, "Mv");
		Proj = glGetUniformLocation(p, "Proj");

		glEnable(GL_DEPTH_TEST);
		glClearColor(1.0, 1.0, 1.0, 1.0);
	}



	int main(int argc, char **argv) {

		// init GLUT and create Window
		glutInit(&argc, argv);
		glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
		glutInitWindowPosition(100,100);
		glutInitWindowSize(600,600);
		glutCreateWindow("COSE436 - Assignment 1");

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
		projPerspective();

		// enter GLUT event processing cycle
		glutMainLoop();

		return 1;
	}