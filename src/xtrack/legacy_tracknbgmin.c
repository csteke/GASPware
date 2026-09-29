#include <stdio.h>
#include <stdlib.h>

void autobgmin_(const float *sp0, float *sb0, const int *n ,const int *istart, const int *iend,
              const int *m, const int *itmax, const float *fstep)
{
	auto int lbuf,nch;
	auto long long  rr,rmin_sym,rmin_sym_r, *p,*p_r, r,r_r,rmin_sym_e2,rmin_sym_e2_r;
	auto int ii, i, j, win, win_r, ifstep, *dwin, iter ;
	auto long long *sp, *sb, *buf, *buf_r, *base;
	auto int loop_b,loop_e;

/*  Buffers initialization */
	loop_b=(*istart+*m-1)*(1.+*fstep)+1;
	loop_e=(*n)*(*fstep+1.)+*m;
	lbuf=(*n)*((*fstep)*3.+1.)+3*(*m)+1;
	ifstep=8192.000*(*fstep);

	base=(long long *)calloc( 4*(lbuf+5) , sizeof(long long));

	dwin=(int *)calloc( (lbuf+5) , sizeof(int));

	if(base == NULL){
	  printf(" ERROR - cannot allocate memory for AUTO backgr. subtraction\n");
	  return;
	 }

/*	p=sb;
	p_r=sb+lbuf;
	while(p <= p_r){
	  *p=0;
	  p++;
	}
*/
	sp=base;
	sb=base+lbuf+5;
	buf=sb+lbuf+5;
	buf_r=buf+lbuf+5;
	i=-1;
	nch=*n-1;
	while(++i<=nch)*(sp+i)=*(sp0+i)*100.0000000;
	j=0;
	for(i=nch;i<=lbuf;i++){
	  j++;
	  *(sp+j+nch)=*(sp-j+nch);
	  }
	i=-1;
	while(++i<=lbuf){*(buf_r+i)=*(buf+i)=*(sp+i); *(dwin+i)=0;}
	
	
/*   Iterations  */
	  for(iter=1;iter<=*itmax;iter++){
	     j=loop_e+1;
	     for(i=loop_b;i<=loop_e;i++){
		j--;
		win=(i > *n)?(*m+ifstep*( ((*n)<<1) -i-loop_b )>>13 ):(*(dwin+i)?*(dwin+i):*m+ifstep*(i-loop_b)>>13);
		win_r=(j > *n)?(*m+ifstep*( ((*n)<<1) -j-loop_b)>>13 ):(*(dwin+j)?*(dwin+j):*m+ifstep*(j-loop_b)>>13);
/*		win=(*(dwin+i))?(*(dwin+i)):*m+(*fstep)*(i-loop_b);
		win_r=(*(dwin+j))?(*(dwin+j)):*m+(*fstep)*(j-loop_b);
		if(i > *n)win=*m+(*fstep)*( ((*n)<<1) -i-loop_b);
		if(j > *n)win_r=*m+(*fstep)*( ((*n)<<1) -j-loop_b); */
/*	printf(" %d  %d --- %d  %d\n",win,*(dwin+i),win_r,*(dwin+j));*/
		p=buf+i;
		p_r=buf_r+j;
		rmin_sym= *(p-1)+*(p+1);
		rmin_sym_r= *(p_r-1)+*(p_r+1);
		*(dwin+i)=*(dwin+j)=1;
		r = ( win < win_r )?win:win_r;
		for(ii=2;ii<=r;ii++){
		   rr = *(p-ii)+*(p+ii);
		   if( rmin_sym >= rr ){
		    rmin_sym = rr;
		    *(dwin+i)=ii;
		    }
		   rr = *(p_r-ii)+*(p_r+ii);
		   if( rmin_sym_r >= rr  ){
		    rmin_sym_r = rr;
		    *(dwin+j)=ii;
		    }
		   }
		if(win > r){
		 for(ii=r+1; ii<=win; ii++){
		   rr = *(p-ii)+*(p+ii);
		   if( rmin_sym >= rr ){
		    rmin_sym = rr;
		    *(dwin+i)=ii;
		    }
		   }
		  }
		else {
		 for(ii=r+1; ii<=win_r; ii++){
		   rr = *(p_r-ii)+*(p_r+ii);
		   if( rmin_sym_r >= rr  ){
		    rmin_sym_r = rr;
		    *(dwin+j)=ii;
		    }
		   }
		  }
/*		   r=*(p-ii)+*(p+ii);
		   ( r <= rmin_sym )?( (rmin_sym=r),
		                             (rmin_sym_e2=(*(sp+i-ii)+*(sp+i+ii)+*(sb+i-ii)+
					     *(sb+i+ii))>>2+*(sp+i)+*(sb+i)),
					     (*(dwin+i)=ii) ):0;
		   ( *(dwin+i) )?( *(dwin+i) ):( *(dwin+i) = 1 ); 
		   }
		   if(r <= rmin_sym){
		     rmin_sym=r;
		     *(dwin+i)=ii;
		     }		   
		   }
		 for(ii=2;ii<=win_r;ii++){
		   ( rmin_sym_r >= *(p_r-ii)+*(p_r+ii) )?(rmin_sym_r = *(p_r-ii)+*(p_r+ii),
		                                          *(dwin+j)=ii):0;
		   r_r=*(p_r-ii)+*(p_r+ii);
		   ( r_r <= rmin_sym_r )?( (rmin_sym_r=r_r),
		                             (rmin_sym_e2_r=(*(sp+j-ii)+*(sp+j+ii)+*(sb+j-ii)+
					     *(sb+j+ii))>>2+*(sp+j)+*(sb+j)),
					     (*(dwin+j)=ii) ):0;
		   ( *(dwin+j) )?( *(dwin+j) ):( *(dwin+j) = 1 );
		   } 
		   if(r_r <= rmin_sym_r){
		     rmin_sym_r=r_r;
		     *(dwin+j)=ii;
		     } 
		   }*/
		 
/*  Replace first order minimum with second order minimum when is statisticaly possible  */

		 
		 rmin_sym>>=1;
		 rmin_sym_r>>=1;
		 rmin_sym_e2=(*(sp+i)+*(sb+i)+rmin_sym)>>2;
		 rmin_sym_e2_r=(*(sp+j)+*(sb+j)+rmin_sym_r)>>2;
		 r=*p-rmin_sym;
		 r_r=*p_r-rmin_sym_r;
/*		 *p = ( r > 0 )? rmin_sym :( ((r*r) <=rmin_sym_e2)?rmin_sym:*p );
		 *p_r = ( r_r > 0 )? rmin_sym_r :( ((r_r*r_r) <=rmin_sym_e2_r)?rmin_sym_r:*p_r );
*/
		 if(r > 0){
		   *p=rmin_sym; }
		 else{
		   if((r*r) < rmin_sym_e2)*p=rmin_sym; 
		 }
		 if(r_r > 0){
		   *p_r=rmin_sym_r; }
		 else{
		   if((r_r*r_r) < rmin_sym_e2_r)*p_r=rmin_sym_r; 
		 }
		   
	      }

	   for(i=loop_b;i<=lbuf;i++){
	      *(sb+i)+=(*(buf+i)+*(buf_r+i))>>1;
	      *(buf_r+i)=*(buf+i)= (*(sp+i)>*(sb+i))?(*(sp+i)-*(sb+i)):0;
/*	      if(*(buf+i) < 0)*(buf+i)=0;
	      *(buf_r+i)=*(buf+i); */
	      
	      }
	  }
	  ii=-1;
	  while(++ii<=loop_b)*(sb+ii)=*(sp+ii);
/* Kill spikes :   */
  i=-1;
  while(++i<=lbuf)*(buf_r+i)=*(buf+i)=*(sb+i);
 	     j=loop_e+1;
	     for(i=loop_b;i<=loop_e;i++){
		j--;
		win=(i > *n)?*m+ifstep*( ((*n)<<1) -i-loop_b)>>13:*m+ifstep*(i-loop_b)>>13;
		win_r=(j > *n)?*m+ifstep*( ((*n)<<1) -j-loop_b)>>13:*m+ifstep*(j-loop_b)>>13;
/*		win=*m+(*fstep)*(i-loop_b);
		win_r=*m+(*fstep)*(j-loop_b);
		if(i > *n)win=*m+(*fstep)*( ((*n)<<1) -i-loop_b);
		if(j > *n)win_r=*m+(*fstep)*( ((*n)<<1) -j-loop_b);
*/
		p=buf+i;
		p_r=buf_r+j;
		rmin_sym= *(p-1)+*(p+1);
		rmin_sym_r= *(p_r-1)+*(p_r+1);
		*(dwin+i)=*(dwin+j)=1;
		r = ( win < win_r )?win:win_r;
		for(ii=2;ii<=r;ii++){
		   rr = *(p-ii)+*(p+ii);
		   if( rmin_sym >= rr ){
		    rmin_sym = rr ;
		    }
		   rr = *(p_r-ii)+*(p_r+ii);
		   if( rmin_sym_r >= rr ){
		    rmin_sym_r = rr;
		    }
		   }
		if(win > r){
		 for(ii=r+1; ii<=win; ii++){
		   rr = *(p-ii)+*(p+ii);
		   if( rmin_sym >= rr ){
		    rmin_sym = rr;
		    }
		   }
		  }
		else {
		 for(ii=r+1; ii<=win_r; ii++){
		   if( rmin_sym_r >= *(p_r-ii)+*(p_r+ii) ){
		    rmin_sym_r = *(p_r-ii)+*(p_r+ii);
		    }
		   }
		  }
/*		rmin_sym=rmin_sym_r=MaxVal;
		p=buf+i;
		p_r=buf_r+j;
		for(ii=1;ii<=win;ii++){
		   r=*(p-ii)+*(p+ii);
		   if(r <= rmin_sym)rmin_sym=r;
		   }
		 for(ii=1;ii<=win_r;ii++){
		   r_r=*(p_r-ii)+*(p_r+ii);
		   if(r_r < rmin_sym_r)rmin_sym_r=r_r;
		   }
*/
		 rmin_sym >>=1;
		 rmin_sym_r >>=1;

		 if(*p > rmin_sym)*p=rmin_sym;
		 if(*p_r > rmin_sym_r)*p_r=rmin_sym_r;

                }
 i=-1;
 while(++i<=nch){ *(sb0+i)=(*(buf+i)+*(buf_r+i))>>1; *(sb0+i)/=100.000000; }
 base=realloc(base,0);
 dwin=realloc(dwin,0); 
}
