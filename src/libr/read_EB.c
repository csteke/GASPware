# include <stdio.h>
# include <iostream.h>
# include <fstream.h>
# include <string.h>
#define k1 1024
#define k2 2048 
#define k4 4096
#define k8 8192
class byte
{
public:
	int liczba;
	byte(int);
	char low();
	char high();
};
byte::byte(int li)
{
	liczba=li;
};
char byte::low()
{
	char c;
	c=(char)liczba;
	//cout<<c<<"\n";
	return c;
};
char byte::high()
{
	char c;
	int nl;
	nl= liczba >> 8;
	c=(char)nl;
	//cout<<c<<"\n";
	return c;
};

enum format {asc,asc_1, bin, spe, gar};

class spectrum
{
public:	
	long *spc;
	int spnumber;
static int counter;
public:	
	
	spectrum(int);
	~spectrum();
	int length;
	int read(format, char *);
	int write(format, char *);
	int fill(int, int, int);
	int list(int, int);
	int multiplay(int);
	int subtract_const(int);
	int smooth(int);
	long * operator+( spectrum &);
	void operator=(long *);

};

int spectrum::counter=0;

spectrum::spectrum(int x)
{
	length = x;
	spc= new long [length];
	if(spc != NULL)
	{
		counter++;
		spnumber=counter;
		//cout << "\n const "<< counter <<  "\n";
		
		
	}
	else
		cout << "cannot create spectrum";
	for(int i=0;i<length;i++)
		spc[i]=0;
	return;
};
spectrum::~spectrum()
{ 	delete spc;
	//cout << "\n destr " << spnumber << "\n";
	return;
};


spectrum::read(format x, char *fname )
{	
	
	ifstream file( fname);
	if(!file) 
	{
		cout << "cannot open file" << fname << "\n";
		return 0;
	}
	
	if(x==bin)
	{
		file.read((char *)spc,(sizeof(long))*length);
		file.close();
		return 1;
	}
	if(x==asc)
	{
		for(int i=0; i<length;i++)
			file >> spc[i];
		file.close();
		return 2;

	}
	if(x==asc_1)
	{
		for(int i=0; i<length;i++)
		{	int tmp;
			file >> tmp;
			file >> spc[i];
		}
		file.close();
		return 2;

	}

	if(x==spe)
	{	char buf[40];	
		float *tmp;
		tmp= new float[length];
		file.read(buf,36);
		file.read((char *)tmp,sizeof(float)*length);
		for(int i=0;i<length;i++)
			spc[i]=(long)tmp[i];
		delete tmp;
		file.close();
		return 3;
	}
	
	if(x==gar)
	{	char buf[500];
	
		file.read(buf,500);
		file.read((char *)spc,sizeof(long)*length);
		file.close();
			return 4;
	}

};
	
spectrum::write(format x, char *fname)
{	
	ofstream file( fname );
	if(!file) cout << "cannot open file" << fname << "\n";
	
	if(x==bin)
	{
		
		file.write((char *)spc, length*sizeof(int));
		file.close();		
		return 1;
	}
	if(x==asc)
	{
		
		for(int i=0; i<length;i++)
			file<<spc[i]<<"\n";
		file.close();
		return 1;

	}
	if(x==spe)
	{		
		byte licz=length;
		float *tmp;
		tmp= new float[length];
		for(int i=0;i<length;i++)
			tmp[i]=(float)spc[i];
			char buf[]={00,00,00,030,
			0141,0141,0141,0141,0141,0141,0141,0141,
			00,00,
			020,00,
			00,00,00,01,00,00,00,01,00,00,00,01,
			00,00,00,030,00,00,0100,00};
		
		strncpy(buf+4,fname,8);
		buf[14]=licz.high();
		buf[15]=licz.low();
		file.write(buf,36);
		file.write((char*)tmp,length*sizeof(float));
		file.close();
		delete tmp;
		return 1;
	}
	if(x==gar)
		cout<<"not yet implemented \n";



};
spectrum::fill(int lc, int hc, int contents)
{
	for( int i=lc; i<=hc; i++)
	 	spc[i]=contents;
	return 1;
};
spectrum::smooth(int par)
{
	for( int i=0; i<length-par;i++)
	{
		float sum=0;
		for(int ii=0;ii<par;ii++)
			sum+=spc[i];
		spc[i]=(int)( sum/par );
	}
	return 0;
}


spectrum::list(int lc, int hc)
{
	for( int i=lc; i<=hc; i++)
	 	cout<< i << ' ' << spc[i] << "\n";
	return 1; 
};
spectrum::multiplay(int factor)
{
	for( int i=0; i< length; i++ )
		spc[i] *= factor;
	return 1;
};
spectrum::subtract_const(int constant)
{
	for( int i=0; i< length; i++ )
		spc[i] -= constant;
	return 1;
};
long * spectrum::operator+( spectrum & drugie)
{
	long *sum;
	sum=new long[length];
	if( drugie.length != length)
	{
		cout << "different spectra lengths";
		return 0;
	};
	
	for( int i=0; i< length; i++)
		sum[i] = spc[i] + drugie.spc[i]; 
	return sum;
};
void spectrum::operator=(long *res)
{	for(int i=0; i<length; i++)
		spc[i]=res[i];
	delete res;
	return;
};
	


long * read_EB(char* name)

{
	
	spectrum s1(8192);

	if( !s1.read(gar,name)) return NULL;
	
	return s1.spc;



}
	
/*
int main()
{
	long *spect;
	spect=read_EB("G4.4M");

	for(int i=100;i<300;i++)
		cout<<i<<" "<<spect[i]<<endl;
}	
*/
