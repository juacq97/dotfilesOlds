diff --git a/config.def.h b/config.def.h
index 7054c06..4448d46 100644
--- a/config.def.h
+++ b/config.def.h
@@ -11,6 +11,8 @@ static const char normfgcolor[]     = "#bbbbbb";
 static const char selbordercolor[]  = "#005577";
 static const char selbgcolor[]      = "#005577";
 static const char selfgcolor[]      = "#eeeeee";
+static unsigned int baralpha        = 0xd0;
+static unsigned int borderalpha     = OPAQUE;
 static const unsigned int borderpx  = 1;        /* border pixel of windows */
 static const unsigned int snap      = 32;       /* snap pixel */
 static const int showbar            = 1;        /* 0 means no bar */
diff --git a/config.mk b/config.mk
index 4eefb71..de25d2a 100644
--- a/config.mk
+++ b/config.mk
@@ -22,7 +22,7 @@ FREETYPEINC = /usr/include/freetype2
 
 # includes and libs
 INCS = -I${X11INC} -I${FREETYPEINC}
-LIBS = -L${X11LIB} -lX11 ${XINERAMALIBS} ${FREETYPELIBS}
+LIBS = -L${X11LIB} -lX11 ${XINERAMALIBS} ${FREETYPELIBS} -lXrender
 
 # flags
 CPPFLAGS = -D_BSD_SOURCE -D_POSIX_C_SOURCE=2 -DVERSION=\"${VERSION}\" ${XINERAMAFLAGS}
diff --git a/drw.c b/drw.c
index f49200b..12e3ebc 100644
--- a/drw.c
+++ b/drw.c
@@ -61,7 +61,7 @@ utf8decode(const char *c, long *u, size_t clen)
 }
 
 Drw *
-drw_create(Display *dpy, int screen, Window root, unsigned int w, unsigned int h)
+drw_create(Display *dpy, int screen, Window root, unsigned int w, unsigned int h, Visual *visual, unsigned int depth, Colormap cmap)
 {
 	Drw *drw;
 
@@ -71,8 +71,11 @@ drw_create(Display *dpy, int screen, Window root, unsigned int w, unsigned int h
 	drw->root = root;
 	drw->w = w;
 	drw->h = h;
-	drw->drawable = XCreatePixmap(dpy, root, w, h, DefaultDepth(dpy, screen));
-	drw->gc = XCreateGC(dpy, root, 0, NULL);
+	drw->visual = visual;
+	drw->depth = depth;
+	drw->cmap = cmap;
+	drw->drawable = XCreatePixmap(dpy, root, w, h, depth);
+	drw->gc = XCreateGC(dpy, drw->drawable, 0, NULL);
 	drw->fontcount = 0;
 	XSetLineAttributes(dpy, drw->gc, 1, LineSolid, CapButt, JoinMiter);
 
@@ -86,7 +89,7 @@ drw_resize(Drw *drw, unsigned int w, unsigned int h)
 	drw->h = h;
 	if (drw->drawable)
 		XFreePixmap(drw->dpy, drw->drawable);
-	drw->drawable = XCreatePixmap(drw->dpy, drw->root, w, h, DefaultDepth(drw->dpy, drw->screen));
+	drw->drawable = XCreatePixmap(drw->dpy, drw->root, w, h, drw->depth);
 }
 
 void
@@ -180,16 +183,15 @@ drw_font_free(Fnt *font)
 }
 
 Clr *
-drw_clr_create(Drw *drw, const char *clrname)
+drw_clr_create(Drw *drw, const char *clrname, unsigned int alpha)
 {
 	Clr *clr;
 
 	clr = ecalloc(1, sizeof(Clr));
-	if (!XftColorAllocName(drw->dpy, DefaultVisual(drw->dpy, drw->screen),
-	                       DefaultColormap(drw->dpy, drw->screen),
+	if (!XftColorAllocName(drw->dpy, drw->visual, drw->cmap,
 	                       clrname, &clr->rgb))
 		die("error, cannot allocate color '%s'\n", clrname);
-	clr->pix = clr->rgb.pixel;
+	clr->pix = (clr->rgb.pixel & 0x00ffffffU) | (alpha << 24);
 
 	return clr;
 }
@@ -245,9 +247,7 @@ drw_text(Drw *drw, int x, int y, unsigned int w, unsigned int h, const char *tex
 		XSetForeground(drw->dpy, drw->gc, invert ?
 		               drw->scheme->fg->pix : drw->scheme->bg->pix);
 		XFillRectangle(drw->dpy, drw->drawable, drw->gc, x, y, w, h);
-		d = XftDrawCreate(drw->dpy, drw->drawable,
-		                  DefaultVisual(drw->dpy, drw->screen),
-		                  DefaultColormap(drw->dpy, drw->screen));
+		d = XftDrawCreate(drw->dpy, drw->drawable, drw->visual, drw->cmap);
 	}
 
 	curfont = drw->fonts[0];
diff --git a/drw.h b/drw.h
index e3b8515..1fed824 100644
--- a/drw.h
+++ b/drw.h
@@ -30,6 +30,9 @@ typedef struct {
 	Display *dpy;
 	int screen;
 	Window root;
+	Visual *visual;
+	unsigned int depth;
+	Colormap cmap;
 	Drawable drawable;
 	GC gc;
 	ClrScheme *scheme;
@@ -43,7 +46,7 @@ typedef struct {
 } Extnts;
 
 /* Drawable abstraction */
-Drw *drw_create(Display *, int, Window, unsigned int, unsigned int);
+Drw *drw_create(Display *, int, Window, unsigned int, unsigned int, Visual*, unsigned int, Colormap);
 void drw_resize(Drw *, unsigned int, unsigned int);
 void drw_free(Drw *);
 
@@ -55,7 +58,7 @@ void drw_font_getexts(Fnt *, const char *, unsigned int, Extnts *);
 unsigned int drw_font_getexts_width(Fnt *, const char *, unsigned int);
 
 /* Colour abstraction */
-Clr *drw_clr_create(Drw *, const char *);
+Clr *drw_clr_create(Drw *, const char *, unsigned int);
 void drw_clr_free(Clr *);
 
 /* Cursor abstraction */
diff --git a/dwm.c b/dwm.c
index 0362114..17fe373 100644
--- a/dwm.c
+++ b/dwm.c
@@ -57,6 +57,8 @@
 #define TAGMASK                 ((1 << LENGTH(tags)) - 1)
 #define TEXTW(X)                (drw_text(drw, 0, 0, 0, 0, (X), 0) + drw->fonts[0]->h)
 
+#define OPAQUE                  0xffU
+
 /* enums */
 enum { CurNormal, CurResize, CurMove, CurLast }; /* cursor */
 enum { SchemeNorm, SchemeSel, SchemeLast }; /* color schemes */
@@ -232,6 +234,7 @@ static Monitor *wintomon(Window w);
 static int xerror(Display *dpy, XErrorEvent *ee);
 static int xerrordummy(Display *dpy, XErrorEvent *ee);
 static int xerrorstart(Display *dpy, XErrorEvent *ee);
+static void xinitvisual();
 static void zoom(const Arg *arg);
 
 /* variables */
@@ -267,6 +270,11 @@ static Drw *drw;
 static Monitor *mons, *selmon;
 static Window root;
 
+static int useargb = 0;
+static Visual *visual;
+static int depth;
+static Colormap cmap;
+
 /* configuration, allows nested code to access above variables */
 #include "config.h"
 
@@ -1556,7 +1564,8 @@ setup(void)
 	sw = DisplayWidth(dpy, screen);
 	sh = DisplayHeight(dpy, screen);
 	root = RootWindow(dpy, screen);
-	drw = drw_create(dpy, screen, root, sw, sh);
+	xinitvisual();
+	drw = drw_create(dpy, screen, root, sw, sh, visual, depth, cmap);
 	drw_load_fonts(drw, fonts, LENGTH(fonts));
 	if (!drw->fontcount)
 		die("no fonts could be loaded.\n");
@@ -1580,12 +1589,12 @@ setup(void)
 	cursor[CurResize] = drw_cur_create(drw, XC_sizing);
 	cursor[CurMove] = drw_cur_create(drw, XC_fleur);
 	/* init appearance */
-	scheme[SchemeNorm].border = drw_clr_create(drw, normbordercolor);
-	scheme[SchemeNorm].bg = drw_clr_create(drw, normbgcolor);
-	scheme[SchemeNorm].fg = drw_clr_create(drw, normfgcolor);
-	scheme[SchemeSel].border = drw_clr_create(drw, selbordercolor);
-	scheme[SchemeSel].bg = drw_clr_create(drw, selbgcolor);
-	scheme[SchemeSel].fg = drw_clr_create(drw, selfgcolor);
+	scheme[SchemeNorm].border = drw_clr_create(drw, normbordercolor, borderalpha);
+	scheme[SchemeNorm].bg = drw_clr_create(drw, normbgcolor, baralpha);
+	scheme[SchemeNorm].fg = drw_clr_create(drw, normfgcolor, OPAQUE);
+	scheme[SchemeSel].border = drw_clr_create(drw, selbordercolor, borderalpha);
+	scheme[SchemeSel].bg = drw_clr_create(drw, selbgcolor, baralpha);
+	scheme[SchemeSel].fg = drw_clr_create(drw, selfgcolor, OPAQUE);
 	/* init bars */
 	updatebars();
 	updatestatus();
@@ -1798,15 +1807,17 @@ updatebars(void)
 	Monitor *m;
 	XSetWindowAttributes wa = {
 		.override_redirect = True,
-		.background_pixmap = ParentRelative,
+		.background_pixel = 0,
+		.border_pixel = 0,
+		.colormap = cmap,
 		.event_mask = ButtonPressMask|ExposureMask
 	};
 	for (m = mons; m; m = m->next) {
 		if (m->barwin)
 			continue;
-		m->barwin = XCreateWindow(dpy, root, m->wx, m->by, m->ww, bh, 0, DefaultDepth(dpy, screen),
-		                          CopyFromParent, DefaultVisual(dpy, screen),
-		                          CWOverrideRedirect|CWBackPixmap|CWEventMask, &wa);
+		m->barwin = XCreateWindow(dpy, root, m->wx, m->by, m->ww, bh, 0, depth,
+		                          InputOutput, visual,
+		                          CWOverrideRedirect|CWBackPixel|CWBorderPixel|CWColormap|CWEventMask, &wa);
 		XDefineCursor(dpy, m->barwin, cursor[CurNormal]->cursor);
 		XMapRaised(dpy, m->barwin);
 	}
@@ -2107,6 +2118,43 @@ xerrorstart(Display *dpy, XErrorEvent *ee)
 }
 
 void
+xinitvisual()
+{
+	XVisualInfo *infos;
+	XRenderPictFormat *fmt;
+	int nitems;
+	int i;
+
+	XVisualInfo tpl = {
+		.screen = screen,
+		.depth = 32,
+		.class = TrueColor
+	};
+	long masks = VisualScreenMask | VisualDepthMask | VisualClassMask;
+
+	infos = XGetVisualInfo(dpy, masks, &tpl, &nitems);
+	visual = NULL;
+	for(i = 0; i < nitems; i ++) {
+		fmt = XRenderFindVisualFormat(dpy, infos[i].visual);
+		if (fmt->type == PictTypeDirect && fmt->direct.alphaMask) {
+			visual = infos[i].visual;
+			depth = infos[i].depth;
+			cmap = XCreateColormap(dpy, root, visual, AllocNone);
+			useargb = 1;
+			break;
+		}
+	}
+
+	XFree(infos);
+
+	if (! visual) {
+		visual = DefaultVisual(dpy, screen);
+		depth = DefaultDepth(dpy, screen);
+		cmap = DefaultColormap(dpy, screen);
+	}
+}
+
+void
 zoom(const Arg *arg)
 {
 	Client *c = selmon->sel;
