*deck @(#)rt123.f	5.1  11/6/94
      subroutine rt123(nroots,vx,vu,vw,n)
c             *****   version february 13,1975   *****
c
c     roots and weights for rys quadrature for 1,2 or 3 roots
c
c
      implicit real*8 (a-h,o-z)
c
      real*8 vx(n),vu(nroots,n),vw(nroots,n)
      real*8 x,u(3),w(3)
c
c
      data r12,pie4/2.75255128608411d-01, 7.85398163397448d-01/
      data r22,w22/ 2.72474487139158d+00, 9.17517095361369d-02/
      data r13/     1.90163509193487d-01/
      data r23,w23/ 1.78449274854325d+00, 1.77231492083829d-01/
      data r33,w33/ 5.52534374226326d+00, 5.11156880411248d-03/
      save r12,pie4,r22,w22,r13,r23,w23,r33,w33
c
c     ----- loop through elements in vector -----
c
      do 1000 i=1,n
         x=vx(i)
c
      if(x.gt.5.0d+00) go to 50
      if(x.gt.1.0d+00) go to 30
      if(x.gt.3.0d-07) go to 20
c     x is approximately zero.         nroots=1,2, or 3
      if(nroots-2) 11,12,13
   11 u(1)=0.5d+00 -x/5.0d+00
      w(1)=1.0d+00 -x/3.0d+00
      go to 998
   12 u(1)=1.30693606237085d-01    -2.90430236082028d-02 *x
      u(2)=2.86930639376291d+00    -6.37623643058102d-01 *x
      w(1)=6.52145154862545d-01    -1.22713621927067d-01 *x
      w(2)=3.47854845137453d-01    -2.10619711404725d-01 *x
      go to 998
   13 u(1)=6.03769246832797d-02    -9.28875764357368d-03 *x
      u(2)=7.76823355931043d-01    -1.19511285527878d-01 *x
      u(3)=6.66279971938567d+00    -1.02504611068957d+00 *x
      w(1)=4.67913934572691d-01    -5.64876917232519d-02 *x
      w(2)=3.60761573048137d-01    -1.49077186455208d-01 *x
      w(3)=1.71324492379169d-01    -1.27768455150979d-01 *x
      go to 998
c     x =0.0 to 1.0                   nroots=1,2, or 3
   20 if(nroots.eq.3) go to 23
      f1=         ((((((((-8.36313918003957d-08*x+1.21222603512827d-06
     1)*x-1.15662609053481d-05 )*x+9.25197374512647d-05
     2)*x-6.40994113129432d-04 )*x+3.78787044215009d-03
     3)*x-1.85185172458485d-02 )*x+7.14285713298222d-02
     4)*x-1.99999999997023d-01 )*x+3.33333333333318d-01
      w(1)=(x+x)*f1+exp(-x)
      if(nroots.eq.2) go to 22
      u(1)=f1/(w(1)-f1)
      go to 998
   22 u(1)=          (((((((-2.35234358048491d-09*x+2.49173650389842d-08
     1)*x-4.558315364581d-08)*x-2.447252174587d-06)*x+4.743292959463d-05
     2)*x-5.33184749432408d-04 )*x+4.44654947116579d-03
     3)*x-2.90430236084697d-02 )*x+1.30693606237085d-01
      u(2)=          (((((((-2.47404902329170d-08*x+2.36809910635906d-07
     1)*x+1.835367736310d-06)*x-2.066168802076d-05)*x-1.345693393936d-04
     2)*x-5.88154362858038d-05 )*x+5.32735082098139d-02
     3)*x-6.37623643056745d-01 )*x+2.86930639376289d+00
      w(2)=((f1-w(1))*u(1)+f1)*(1.0d+00+u(2))/(u(2)-u(1))
      w(1)=w(1)-w(2)
      go to 998
   23 u(1)=           ((((((-5.10186691538870d-10*x+2.40134415703450d-08
     1)*x-5.01081057744427d-07 )*x+7.58291285499256d-06
     2)*x-9.55085533670919d-05 )*x+1.02893039315878d-03
     3)*x-9.28875764374337d-03 )*x+6.03769246832810d-02
      u(2)=           ((((((-1.29646524960555d-08*x+7.74602292865683d-08
     1)*x+1.56022811158727d-06 )*x-1.58051990661661d-05
     2)*x-3.30447806384059d-04 )*x+9.74266885190267d-03
     3)*x-1.19511285526388d-01 )*x+7.76823355931033d-01
      u(3)=           ((((((-9.28536484109606d-09*x-3.02786290067014d-07
     1)*x-2.50734477064200d-06 )*x-7.32728109752881d-06
     2)*x+2.44217481700129d-04 )*x+4.94758452357327d-02
     3)*x-1.02504611065774d+00 )*x+6.66279971938553d+00
      f2=         ((((((((-7.60911486098850d-08*x+1.09552870123182d-06
     1)*x-1.03463270693454d-05 )*x+8.16324851790106d-05
     2)*x-5.55526624875562d-04 )*x+3.20512054753924d-03
     3)*x-1.51515139838540d-02 )*x+5.55555554649585d-02
     4)*x-1.42857142854412d-01 )*x+1.99999999999986d-01
  300 e=exp(-x)
      f1=((x+x)*f2+e)/3.0d+00
      w(1)=(x+x)*f1+e
  301 t1=u(1)/(u(1)+1.0d+00)
      t2=u(2)/(u(2)+1.0d+00)
      t3=u(3)/(u(3)+1.0d+00)
      a2=f2-t1*f1
      a1=f1-t1*w(1)
      w(3)=(a2-t2*a1)/((t3-t2)*(t3-t1))
      w(2)=(t3*a1-a2)/((t3-t2)*(t2-t1))
      w(1)=w(1)-w(2)-w(3)
      go to 998
   30 if(x.gt.3.0d+00) go to 40
c     x =1.0 to 3.0                   nroots=1,2, or 3
      y=x-2.0d+00
      if(nroots.eq.3) go to 33
      f1=       ((((((((((-1.61702782425558d-10*y+1.96215250865776d-09
     1)*y-2.14234468198419d-08 )*y+2.17216556336318d-07
     2)*y-1.98850171329371d-06 )*y+1.62429321438911d-05
     3)*y-1.16740298039895d-04 )*y+7.24888732052332d-04
     4)*y-3.79490003707156d-03 )*y+1.61723488664661d-02
     5)*y-5.29428148329736d-02 )*y+1.15702180856167d-01
      w(1)=(x+x)*f1+exp(-x)
      if(nroots.eq.2) go to 32
      u(1)=f1/(w(1)-f1)
      go to 998
   32 u(1)=        (((((((((-6.36859636616415d-12*y+8.47417064776270d-11
     1)*y-5.152207846962d-10)*y-3.846389873308d-10)*y+8.472253388380d-08
     2)*y-1.85306035634293d-06 )*y+2.47191693238413d-05
     3)*y-2.49018321709815d-04 )*y+2.19173220020161d-03
     4)*y-1.63329339286794d-02 )*y+8.68085688285261d-02
      u(2)=        ((((((((( 1.45331350488343d-10*y+2.07111465297976d-09
     1)*y-1.878920917404d-08)*y-1.725838516261d-07)*y+2.247389642339d-06
     2)*y+9.76783813082564d-06 )*y-1.93160765581969d-04
     3)*y-1.58064140671893d-03 )*y+4.85928174507904d-02
     4)*y-4.30761584997596d-01 )*y+1.80400974537950d+00
      w(2)=((f1-w(1))*u(1)+f1)*(1.0d+00+u(2))/(u(2)-u(1))
      w(1)=w(1)-w(2)
      go to 998
   33 u(1)=         (((((((( 1.44687969563318d-12*y+4.85300143926755d-12
     1)*y-6.55098264095516d-10 )*y+1.56592951656828d-08
     2)*y-2.60122498274734d-07 )*y+3.86118485517386d-06
     3)*y-5.13430986707889d-05 )*y+6.03194524398109d-04
     4)*y-6.11219349825090d-03 )*y+4.52578254679079d-02
      u(2)=          ((((((( 6.95964248788138d-10*y-5.35281831445517d-09
     1)*y-6.745205954533d-08)*y+1.502366784525d-06)*y+9.923326947376d-07
     2)*y-3.89147469249594d-04 )*y+7.51549330892401d-03
     3)*y-8.48778120363400d-02 )*y+5.73928229597613d-01
      u(3)=         ((((((((-2.81496588401439d-10*y+3.61058041895031d-09
     1)*y+4.53631789436255d-08 )*y-1.40971837780847d-07
     2)*y-6.05865557561067d-06 )*y-5.15964042227127d-05
     3)*y+3.34761560498171d-05 )*y+5.04871005319119d-02
     4)*y-8.24708946991557d-01 )*y+4.81234667357205d+00
      f2=       ((((((((((-1.48044231072140d-10*y+1.78157031325097d-09
     1)*y-1.92514145088973d-08 )*y+1.92804632038796d-07
     2)*y-1.73806555021045d-06 )*y+1.39195169625425d-05
     3)*y-9.74574633246452d-05 )*y+5.83701488646511d-04
     4)*y-2.89955494844975d-03 )*y+1.13847001113810d-02
     5)*y-3.23446977320647d-02 )*y+5.29428148329709d-02
      go to 300
c     x =3.0 to 5.0                   nroots =1,2, or 3
   40 y=x-4.0d+00
      if(nroots.eq.3) go to 43
      f1=       ((((((((((-2.62453564772299d-11*y+3.24031041623823d-10
     1)*y-3.614965656163d-09)*y+3.760256799971d-08)*y-3.553558319675d-07
     2)*y+3.022556449731d-06)*y-2.290098979647d-05)*y+1.526537461148d-04
     3)*y-8.81947375894379d-04 )*y+4.33207949514611d-03
     4)*y-1.75257821619926d-02 )*y+5.28406320615584d-02
      w(1)=(x+x)*f1+exp(-x)
      if(nroots.eq.2) go to 42
      u(1)=f1/(w(1)-f1)
      go to 998
   42 u(1)=         ((((((((-4.11560117487296d-12*y+7.10910223886747d-11
     1)*y-1.73508862390291d-09 )*y+5.93066856324744d-08
     2)*y-9.76085576741771d-07 )*y+1.08484384385679d-05
     3)*y-1.12608004981982d-04 )*y+1.16210907653515d-03
     4)*y-9.89572595720351d-03 )*y+6.12589701086408d-02
      u(2)=        (((((((((-1.80555625241001d-10*y+5.44072475994123d-10
     1)*y+1.603498045240d-08)*y-1.497986283037d-07)*y-7.017002532106d-07
     2)*y+1.85882653064034d-05 )*y-2.04685420150802d-05
     3)*y-2.49327728643089d-03 )*y+3.56550690684281d-02
     4)*y-2.60417417692375d-01 )*y+1.12155283108289d+00
      w(2)=((f1-w(1))*u(1)+f1)*(1.0d+00+u(2))/(u(2)-u(1))
      w(1)=w(1)-w(2)
      go to 998
   43 u(1)=          ((((((( 1.44265709189601d-11*y-4.66622033006074d-10
     1)*y+7.649155832025d-09)*y-1.229940017368d-07)*y+2.026002142457d-06
     2)*y-2.87048671521677d-05 )*y+3.70326938096287d-04
     3)*y-4.21006346373634d-03 )*y+3.50898470729044d-02
      u(2)=         ((((((((-2.65526039155651d-11*y+1.97549041402552d-10
     1)*y+2.15971131403034d-09 )*y-7.95045680685193d-08
     2)*y+5.15021914287057d-07 )*y+1.11788717230514d-05
     3)*y-3.33739312603632d-04 )*y+5.30601428208358d-03
     4)*y-5.93483267268959d-02 )*y+4.31180523260239d-01
      u(3)=         ((((((((-3.92833750584041d-10*y-4.16423229782280d-09
     1)*y+4.42413039572867d-08 )*y+6.40574545989551d-07
     2)*y-3.05512456576552d-06 )*y-1.05296443527943d-04
     3)*y-6.14120969315617d-04 )*y+4.89665802767005d-02
     4)*y-6.24498381002855d-01 )*y+3.36412312243724d+00
      f2=       ((((((((((-2.36788772599074d-11*y+2.89147476459092d-10
     1)*y-3.18111322308846d-09 )*y+3.25336816562485d-08
     2)*y-3.00873821471489d-07 )*y+2.48749160874431d-06
     3)*y-1.81353179793672d-05 )*y+1.14504948737066d-04
     4)*y-6.10614987696677d-04 )*y+2.64584212770942d-03
     5)*y-8.66415899015349d-03 )*y+1.75257821619922d-02
      go to 300
   50 if(x.gt.15.0d+00) go to 70
      e=exp(-x)
      if(x.gt.10.0d+00) go to 60
c     x =5.0 to 10.0                  nroots =1,2, or 3
      w(1)=   (((((( 4.6897511375022d-01/x-6.9955602298985d-01)/x
     1+5.3689283271887d-01)/x-3.2883030418398d-01)/x
     2+2.4645596956002d-01)/x-4.9984072848436d-01)/x
     3-3.1501078774085d-06)*e + sqrt(pie4/x)
      f1=(w(1)-e)/(x+x)
      if(nroots-2) 51,52,53
   51 u(1)=f1/(w(1)-f1)
      go to 998
   52 y=x-7.5d+00
      u(1)=    (((((((((((((-1.43632730148572d-16*y+2.38198922570405d-16
     1)*y+1.358319618800d-14)*y-7.064522786879d-14)*y-7.719300212748d-13
     2)*y+7.802544789997d-12)*y+6.628721099436d-11)*y-1.775564159743d-09
     3)*y+1.713828823990d-08)*y-1.497500187053d-07)*y+2.283485114279d-06
     4)*y-3.76953869614706d-05 )*y+4.74791204651451d-04
     5)*y-4.60448960876139d-03 )*y+3.72458587837249d-02
      u(2)=     (((((((((((( 2.48791622798900d-14*y-1.36113510175724d-13
     1)*y-2.224334349799d-12)*y+4.190559455515d-11)*y-2.222722579924d-10
     2)*y-2.624183464275d-09)*y+6.128153450169d-08)*y-4.383376014528d-07
     3)*y-2.49952200232910d-06 )*y+1.03236647888320d-04
     4)*y-1.44614664924989d-03 )*y+1.35094294917224d-02
     5)*y-9.53478510453887d-02 )*y+5.44765245686790d-01
      w(2)=((f1-w(1))*u(1)+f1)*(1.0d+00+u(2))/(u(2)-u(1))
      w(1)=w(1)-w(2)
      go to 998
   53 f2=(f1+f1+f1-e)/(x+x)
      y=x-7.5d+00
      u(1)=      ((((((((((( 5.74429401360115d-16*y+7.11884203790984d-16
     1)*y-6.736701449826d-14)*y-6.264613873998d-13)*y+1.315418927040d-11
     2)*y-4.23879635610964d-11 )*y+1.39032379769474d-09
     3)*y-4.65449552856856d-08 )*y+7.34609900170759d-07
     4)*y-1.08656008854077d-05 )*y+1.77930381549953d-04
     5)*y-2.39864911618015d-03 )*y+2.39112249488821d-02
      u(2)=      ((((((((((( 1.13464096209120d-14*y+6.99375313934242d-15
     1)*y-8.595618132088d-13)*y-5.293620408757d-12)*y-2.492175211635d-11
     2)*y+2.73681574882729d-09 )*y-1.06656985608482d-08
     3)*y-4.40252529648056d-07 )*y+9.68100917793911d-06
     4)*y-1.68211091755327d-04 )*y+2.69443611274173d-03
     5)*y-3.23845035189063d-02 )*y+2.75969447451882d-01
      u(3)=     (((((((((((( 6.66339416996191d-15*y+1.84955640200794d-13
     1)*y-1.985141104444d-12)*y-2.309293727603d-11)*y+3.917984522103d-10
     2)*y+1.663165279876d-09)*y-6.205591993923d-08)*y+8.769581622041d-09
     3)*y+8.97224398620038d-06 )*y-3.14232666170796d-05
     4)*y-1.83917335649633d-03 )*y+3.51246831672571d-02
     5)*y-3.22335051270860d-01 )*y+1.73582831755430d+00
      go to 301
c     x =10.0 to 15.0                 nroots=1,2, or 3
   60 w(1)=      (((-1.8784686463512d-01/x+2.2991849164985d-01)/x
     1-4.9893752514047d-01)/x-2.1916512131607d-05)*e + sqrt(pie4/x)
      f1=(w(1)-e)/(x+x)
      if(nroots-2) 61,62,63
   61 u(1)=f1/(w(1)-f1)
      go to 998
   62 u(1)=     ((((-1.01041157064226d-05*x+1.19483054115173d-03)*x
     1 -6.73760231824074d-02)*x+1.25705571069895d+00)*x
     2 +     (((-8.57609422987199d+03/x+5.91005939591842d+03)/x
     3 -1.70807677109425d+03)/x+2.64536689959503d+02)/x
     4 -2.38570496490846d+01)*e + r12/(x-r12)
      u(2)=      ((( 3.39024225137123d-04*x-9.34976436343509d-02)*x
     1 -4.22216483306320d+00)*x +       (((-2.08457050986847d+03/x
     2 -1.04999071905664d+03)/x+3.39891508992661d+02)/x
     3 -1.56184800325063d+02)/x+8.00839033297501d+00)*e + r22/(x-r22)
      w(2)=((f1-w(1))*u(1)+f1)*(1.0d+00+u(2))/(u(2)-u(1))
      w(1)=w(1)-w(2)
      go to 998
   63 f2=(f1+f1+f1-e)/(x+x)
      y=x-12.5d+00
      u(1)=      ((((((((((( 4.42133001283090d-16*y-2.77189767070441d-15
     1)*y-4.084026087887d-14)*y+5.379885121517d-13)*y+1.882093066702d-12
     2)*y-8.67286219861085d-11 )*y+7.11372337079797d-10
     3)*y-3.55578027040563d-09 )*y+1.29454702851936d-07
     4)*y-4.14222202791434d-06 )*y+8.04427643593792d-05
     5)*y-1.18587782909876d-03 )*y+1.53435577063174d-02
      u(2)=      ((((((((((( 6.85146742119357d-15*y-1.08257654410279d-14
     1)*y-8.579165965128d-13)*y+6.642452485783d-12)*y+4.798806828724d-11
     2)*y-1.13413908163831d-09 )*y+7.08558457182751d-09
     3)*y-5.59678576054633d-08 )*y+2.51020389884249d-06
     4)*y-6.63678914608681d-05 )*y+1.11888323089714d-03
     5)*y-1.45361636398178d-02 )*y+1.65077877454402d-01
      u(3)=     (((((((((((( 3.20622388697743d-15*y-2.73458804864628d-14
     1)*y-3.157134329361d-13)*y+8.654129268056d-12)*y-5.625235879301d-11
     2)*y-7.718080513708d-10)*y+2.064664199164d-08)*y-1.567725007761d-07
     3)*y-1.57938204115055d-06 )*y+6.27436306915967d-05
     4)*y-1.01308723606946d-03 )*y+1.13901881430697d-02
     5)*y-1.01449652899450d-01 )*y+7.77203937334739d-01
      go to 301
   70 if(x.gt.33.0d+00) go to 90
c     x =15.0 to 33.0                 nroots=1,2, or 3
      e=exp(-x)
      w(1)=       (( 1.9623264149430d-01/x-4.9695241464490d-01)/x
     1-6.0156581186481d-05)*e + sqrt(pie4/x)
      f1=(w(1)-e)/(x+x)
      if(nroots-2) 71,72,73
   71 u(1)=f1/(w(1)-f1)
      go to 998
   72 u(1)=     ((((-1.14906395546354d-06*x+1.76003409708332d-04)*x
     1 -1.71984023644904d-02)*x-1.37292644149838d-01)*x
     2 +       (-4.75742064274859d+01/x+9.21005186542857d+00)/x
     3 -2.31080873898939d-02)*e + r12/(x-r12)
      u(2)=      ((( 3.64921633404158d-04*x-9.71850973831558d-02)*x
     1 -4.02886174850252d+00)*x +         (-1.35831002139173d+02/x
     2 -8.66891724287962d+01)/x+2.98011277766958d+00)*e + r22/(x-r22)
      w(2)=((f1-w(1))*u(1)+f1)*(1.0d+00+u(2))/(u(2)-u(1))
      w(1)=w(1)-w(2)
      go to 998
   73 f2=(f1+f1+f1-e)/(x+x)
      if(x.gt.20.0d+00) go to 83
      u(1)=   ((((((-2.43270989903742d-06*x+3.57901398988359d-04)*x
     1 -2.34112415981143d-02)*x+7.81425144913975d-01)*x
     2 -1.73209218219175d+01)*x+2.43517435690398d+02)*x
     3 +       (-1.97611541576986d+04/x+9.82441363463929d+03)/x
     4 -2.07970687843258d+03)*e + r13/(x-r13)
      u(2)=    (((((-2.62627010965435d-04*x+3.49187925428138d-02)*x
     1 -3.09337618731880d+00)*x+1.07037141010778d+02)*x
     2 -2.36659637247087d+03)*x +        ((-2.91669113681020d+06/x
     3 +1.41129505262758d+06)/x-2.91532335433779d+05)/x
     4 +3.35202872835409d+04)*e + r23/(x-r23)
      u(3)=    ((((( 9.31856404738601d-05*x-2.87029400759565d-02)*x
     1 -7.83503697918455d-01)*x-1.84338896480695d+01)*x
     2 +4.04996712650414d+02)*x +         (-1.89829509315154d+05/x
     3 +5.11498390849158d+04)/x-6.88145821789955d+03)*e + r33/(x-r33)
      go to 301
   83 u(1)=     ((((-4.97561537069643d-04*x-5.00929599665316d-02)*x
     1 +1.31099142238996d+00)*x-1.88336409225481d+01)*x
     2 -6.60344754467191d+02 /x+1.64931462413877d+02)*e + r13/(x-r13)
      u(2)=     ((((-4.48218898474906d-03*x-5.17373211334924d-01)*x
     1 +1.13691058739678d+01)*x-1.65426392885291d+02)*x
     2 -6.30909125686731d+03 /x+1.52231757709236d+03)*e + r23/(x-r23)
      u(3)=     ((((-1.38368602394293d-02*x-1.77293428863008d+00)*x
     1 +1.73639054044562d+01)*x-3.57615122086961d+02)*x
     2 -1.45734701095912d+04 /x+2.69831813951849d+03)*e + r33/(x-r33)
      go to 301
c     x =33.0 to infinity             nroots=1,2, or 3
   90 w(1)=sqrt(pie4/x)
      if(nroots-2) 91,92,93
   91 u(1)=0.5d+00/(x-0.5d+00)
      go to 998
   92 if(x.gt.40.0d+00) go to 102
      e=exp(-x)
      u(1)=(-8.78947307498880d-01*x+1.09243702330261d+01)*e +r12/(x-r12)
      u(2)=(-9.28903924275977d+00*x+8.10642367843811d+01)*e +r22/(x-r22)
      w(2)=( 4.46857389308400d+00*x-7.79250653461045d+01)*e +w22*w(1)
      w(1)=w(1)-w(2)
      go to 998
   93 if(x.gt.47.0d+00) go to 103
      e=exp(-x)
      u(1)=       ((-7.39058467995275d+00*x+3.21318352526305d+02)*x
     1 -3.99433696473658d+03)*e + r13/(x-r13)
      u(2)=       ((-7.38726243906513d+01*x+3.13569966333873d+03)*x
     1 -3.86862867311321d+04)*e + r23/(x-r23)
      u(3)=       ((-2.63750565461336d+02*x+1.04412168692352d+04)*x
     1 -1.28094577915394d+05)*e + r33/(x-r33)
      w(3)=      ((( 1.52258947224714d-01*x-8.30661900042651d+00)*x
     1 +1.92977367967984d+02)*x-1.67787926005344d+03)*e + w33*w(1)
      w(2)=       (( 6.15072615497811d+01*x-2.91980647450269d+03)*x
     1 +3.80794303087338d+04)*e + w23*w(1)
      w(1)=w(1)-w(2)-w(3)
      go to 998
  102 u(1)=r12/(x-r12)
      u(2)=r22/(x-r22)
      w(2)=w22*w(1)
      w(1)=w(1)-w(2)
      go to 998
  103 u(1)=r13/(x-r13)
      u(2)=r23/(x-r23)
      u(3)=r33/(x-r33)
      w(2)=w23*w(1)
      w(3)=w33*w(1)
      w(1)=w(1)-w(2)-w(3)
c
c     ----- transfer roots and weights to vectors -----
c
  998    continue
         do 999 j=1,nroots
            vu(j,i)=u(j)
            vw(j,i)=w(j)
  999    continue
 1000 continue
c
c
      return
      end