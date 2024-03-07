/*   Calibration Functions                                         */

void CAL_DrawArrow( Int32 x1, Int32 y1, Int32 x2, Int32 y2 ){

  LineStruct *l;
  BPoint p;
  
  if( !BP_IsVisible( b ) )return;
  
  p = b->p;
  while( p ){
    BP_DrawPoint( p );
    l = BP_LineBetween( p, p->Next);
    if( l ){
      logicop(LO_XOR);
      color(GLW_UBANANACOLOR);
      move2i( l->x1, l->y1);
      draw2i( l->x2, l->y2);
      logicop(LO_SRC);
      }
    p = p->Next;
    }
}
