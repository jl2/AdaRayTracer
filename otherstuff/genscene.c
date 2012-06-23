#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void put_header(FILE *of, int numobj, int numlight)
{
	fprintf(of, "%d\n%d\n", numobj, numlight);
	fprintf(of, "camera \n");
	fprintf(of, "0 0 -8\n");
	fprintf(of, "0 0 10\n");
	fprintf(of, "0 1 0\n");
	fprintf(of, "1 0 0\n");
	fprintf(of, "0 0 0\n");
	fprintf(of, "512 512\n");
	fprintf(of, "256 256 256\n");
	fprintf(of, "-9 -9 -9\n");
	fprintf(of, "9 9 9\n");
}

#define PI (3.1415)
#define US (128)
#define VS (128)
#define XROT (-PI/3.0)
#define ZROT (PI/8.0)


double randF(double min, double max)
{
	double randMax = (double)RAND_MAX;
	double range = max-min;
	double temp = (double)rand()/randMax;
	temp = range*temp;
	temp = temp+min;
	return temp;
}
	

int main(int argc, char *argv[])
{
	FILE *outf;
	int i, j, k;
	double xp, yp, zp;
	double xt,yt,zt;
	double u;
	double v;

	/*	Used for rotation	*/
	double m1[9] = {0.0};
	double m2[9] = {0.0};

	double umin, vmin, umax, vmax;
	double r = 4.0;
	umin = -2*PI;
	umax = 2*PI;
	vmin = 0*PI;
	vmax = 2*PI;
	
	
	if (argc < 2)
	{
		printf("No output file specified.\n");
		return 1;
	}
	outf = fopen(argv[1], "wt");
	put_header(outf, US*VS,5);

	fprintf(outf, "lights\n");
	fprintf(outf, "1.0  1.0  1.0  8.0  8.0  8.0\n");
	fprintf(outf, "1.0  1.0  1.0  -8.0  8.0  8.0\n");
	fprintf(outf, "1.0  1.0  1.0  8.0  -8.0  8.0\n");
	fprintf(outf, "1.0  1.0  1.0  -8.0  -8.0  8.0\n");
	fprintf(outf, "-1.0  -1.0  -1.0  0.0  0.0  0.0\n");	
	fprintf(outf, "objects\n");
	
	m1[0]=1.0;
	m1[4]=cos(XROT);
	m1[5]=-sin(XROT);
	m1[7]=sin(XROT);
	m1[8]=cos(XROT);

	m2[0]=cos(ZROT);
	m2[2]=sin(ZROT);
	m2[4]=1.0;
	m2[6]=-sin(ZROT);
	m2[8]=cos(ZROT);
	
	
	u=umin;
	for (i=1;i<=US;++i)
	{
		v=vmin;
		for (j=1;j<=VS;++j)
		{
			fprintf(outf, "\nsphere\n");
			fprintf(outf, "%f\n",1.0/12.0);
			/*xp=4.0*(cos(u)*(cos(0.5*u)*(1.414213562+cos(v))+sin(0.5*u)*sin(v)*cos(v)));
			zp=4.0*(sin(u)*(cos(0.5*u)*(1.414213562+cos(v))+sin(0.5*u)*sin(v)*cos(v)));
			yp=4.0*(-sin(0.5*u)*(1.414213562+cos(v))+cos(0.5*u)*sin(v)*cos(v));
			*/

			xp=4.0*(sinh(v)*cos(r*u))/(1+cosh(u)*cosh(v));
			zp=4.0*(sinh(v)*sin(r*u))/(1+cosh(u)*cosh(v));
			yp=4.0*(cosh(v)*sinh(u))/(1+cosh(u)*cosh(v));
			

			/*xp = u*cos(v);
			zp = u*sin(v);
			yp = v*cos(u);
			*/
			xt=xp*m1[0]+yp*m1[1]+zp*m1[2];
			yt=xp*m1[3]+yp*m1[4]+zp*m1[5];
			zt=xp*m1[6]+yp*m1[7]+zp*m1[8];
			
			xp=xt*m2[0]+yt*m2[1]+zt*m2[2];
			yp=xt*m2[3]+yt*m2[4]+zt*m2[5];
			zp=xt*m2[6]+yt*m2[7]+zt*m2[8];
	
			fprintf(outf, "%f %f %f\n", xp, yp, zp);
			fprintf(outf, "0.2 0.2 0.2\n");
			fprintf(outf, "%f %f %f\n", (xp)/10.0+0.5, (yp)/10.0+0.5,(zp)/10.0+0.5);
			fprintf(outf, "%f %f %f\n", (yp)/10.0, (zp)/10.0,(xp)/10.0);
			fprintf(outf, "0.0 0.0 0.0\n");
			fprintf(outf, "%f %f %f\n", (zp)/10.0+0.5, (xp)/10.0+0.5,(yp)/10.0+0.5);
			fprintf(outf, "1.0\n");
			v = v+(vmax-vmin)/(double)VS;
		}
		u = u+(umax-umin)/(double)US;
	}
	fclose(outf);
	return 0;
}


	
	
