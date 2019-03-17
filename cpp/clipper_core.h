/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  17 March 2019                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Core Clipper Library module                                     *
*              Contains structures and functions used throughout the library   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_CORE_H
#define CLIPPER_CORE_H

#include "stdint.h"
#include <stdlib.h>
#include <algorithm>
#include <cmath>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <ostream>
#include <stdexcept>
#include <vector>

namespace clipperlib {

//cInt could be defined as int here. While this would improve performance, it would
//also limit coordinate values in clipping operations to approximately Sqrt(MaxInt)/2;
typedef int64_t cInt;

// Point -----------------------------------------------------------------------

template <typename T>
struct Point;

typedef Point<cInt> PointI;
typedef Point<double> PointD;

template <typename T>
struct Point {
	T x;
	T y;

	Point(T x = 0, T y = 0) :
		x(x),
		y(y){};

	Point &operator=(const Point &other) {
    if (this != &other) {
      x = other.x;
      y = other.y;
    }		
    return *this;
	}

	void Rotate(const PointD &center, double angle_rad);
	void Rotate(const PointD &center, double sin_a, double cos_a);

	friend inline bool operator==(const Point &a, const Point &b) {
		return a.x == b.x && a.y == b.y;
	}
	friend inline bool operator!=(const Point &a, const Point &b) {
		return a.x != b.x || a.y != b.y;
	}
	friend inline bool operator<(const Point &a, const Point &b) {
		return (a.x == b.x) ? (a.y < b.y) : (a.x < b.x);
	}
	friend std::ostream &operator<<(std::ostream &os, const Point<T> &point) {
		os << "(" << point.x << "," << point.y << ")";
		return os;
	}
};

// Rect ------------------------------------------------------------------------

template <typename T>
struct Rect;

typedef Rect<cInt> RectI;
typedef Rect<double> RectD;

template <typename T>
struct Rect {
	T left;
	T top;
	T right;
	T bottom;

	Rect() :
		left(0),
		top(0),
		right(0),
		bottom(0) {}

	Rect(T l, T t, T r, T b) :
		left(l),
		top(t),
		right(r),
		bottom(b) {}

	Rect &operator=(const Rect &other) {
		if (this != &other) {
			left = other.left;
			top = other.top;
			right = other.right;
			bottom = other.bottom;
		}
		return *this;
	}

	T Width() const { return right - left; }
	T Height() const { return bottom - top; }
	void Width(T _width) { right = left + _width; }
	void Height(T _height) { bottom = top + _height; }

	bool IsEmpty() const { return bottom <= top || right <= left; };

	void Inflate(T dx, T dy) {
		left -= dx;
		right += dx;
		top -= dy;
		bottom += dy;
	}

	void Offset(T dx, T dy) {
		left += dx;
		right += dx;
		top += dy;
		bottom += dy;
	}

	void Intersect(const Rect<T> &rect);
	void Union(const Rect<T> &rect);
	void Rotate(double angle_rad);

	friend std::ostream &operator<<(std::ostream &os, const Rect<T> &rect) {
		os << "("
		   << rect.left << "," << rect.top << "," << rect.right << "," << rect.bottom
		   << ")";
		return os;
	}
};

// Path ------------------------------------------------------------------------

//Path: a simple data structure to represent a series of vertices, whether
//open (poly-line) or closed (polygon). A path may be simple or complex (self
//intersecting). For simple polygons, path orientation (whether clockwise or
//counter-clockwise) is generally used to differentiate outer paths from inner
//paths (holes). For complex polygons (and also for overlapping polygons),
//explicit 'filling rules' (see below) are used to indicate regions that are
//inside (filled) and regions that are outside (unfilled) a specific polygon.

template <typename T>
struct Path;

typedef Path<cInt> PathI;
typedef Path<double> PathD;

template <typename T>
struct Path {
	std::vector<Point<T> > data;

	size_t size() const { return data.size(); }
	void resize(size_t size) { data.resize(size); }
	void reserve(size_t size) { data.reserve(size); }
	void push_back(const Point<T> &point) { data.push_back(point); }
	void clear() { data.clear(); }

	Path() {}
	Path(const PathI &other, double scale_x = 1.0, double scale_y = 1.0);
	Path(const PathD &other, double scale_x = 1.0, double scale_y = 1.0);

	Point<T> &operator[](size_t idx) { return data[idx]; }
	const Point<T> &operator[](size_t idx) const { return data[idx]; }

	Path &operator=(const PathI &other);
	Path &operator=(const PathD &other);

	void Append(const Path<T> &extra);
	double Area() const;
	Rect<T> Bounds() const;
	void Offset(T dx, T dy);
	bool Orientation() const;
	void Reverse();
	void Rotate(const PointD &center, double angle_rad);
	void Scale(T sx, T sy);
	void StripDuplicates();

	friend inline Path<T> &operator<<(Path<T> &path, const Point<T> &point) {
		path.data.push_back(point);
		return path;
	}
	friend std::ostream &operator<<(std::ostream &os, const Path<T> &path) {
		if (path.data.empty())
			return os;

		size_t last = path.size() - 1;

		for (size_t i = 0; i < last; i++)
			os << "(" << path[i].x << "," << path[i].y << "), ";

		os << "(" << path[last].x << "," << path[last].y << ")\n";

		return os;
	}
};

// Paths -----------------------------------------------------------------------

template <typename T>
struct Paths {
	std::vector<Path<T> > data;

	size_t size() const { return data.size(); }
	void resize(size_t size) { data.resize(size); }
	void reserve(size_t size) { data.reserve(size); }
	void push_back(const Path<T> &path) { data.push_back(path); }
	void clear() { data.clear(); }

	Path<T> &operator[](size_t idx) { return data[idx]; }
	const Path<T> &operator[](size_t idx) const { return data[idx]; }

	void Append(const Paths<T> &extra);
	Rect<T> Bounds() const;
	void Offset(T dx, T dy);
	void Reverse();
	void Rotate(const PointD &center, double angle_rad);
	void Scale(T sx, T sy);

	friend inline Paths<T> &operator<<(Paths<T> &paths, const Path<T> &path) {
		paths.data.push_back(path);
		return paths;
	}

	friend std::ostream &operator<<(std::ostream &os, const Paths<T> &paths) {
		for (size_t i = 0; i < paths.size(); i++)
			os << paths[i];
		os << "\n";
		return os;
	}
};

typedef Paths<cInt> PathsI;
typedef Paths<double> PathsD;

// PathsArray ------------------------------------------------------------------

template <typename T>
struct PathsArray {
	std::vector<Paths<T> > data;

	size_t size() const { return data.size(); }
	void resize(size_t size) { data.resize(size); }
	void reserve(size_t size) { data.reserve(size); }
	void push_back(const Paths<T> &paths) { data.push_back(paths); }
	void clear() { data.clear(); }

	Paths<T> &operator[](size_t idx) { return data[idx]; }
	const Paths<T> &operator[](size_t idx) const { return data[idx]; }

	Rect<T> Bounds() const;
};

typedef PathsArray<cInt> PathsArrayI;
typedef PathsArray<double> PathsArrayD;

// Miscellaneous ---------------------------------------------------------------

//Note: all clipping operations except for Difference are commutative.
enum ClipType {
	ctNone,
	ctIntersection,
	ctUnion,
	ctDifference,
	ctXor
};

enum PathType {
	ptSubject,
	ptClip
};

//By far the most widely used filling rules for polygons are EvenOdd
//and NonZero, sometimes called Alternate and Winding respectively.
//https://en.wikipedia.org/wiki/Nonzero-rule
enum FillRule {
	frEvenOdd,
	frNonZero,
	frPositive,
	frNegative
};

//PointInPolygon
enum PipResult {
	pipInside,
	pipOutside,
	pipOnEdge
};

PipResult PointInPolygon(const PointI &pt, const PathI &path);

double CrossProduct(const PointI &pt1, const PointI &pt2, const PointI &pt3);

}  // namespace clipperlib

#endif
