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

#include "clipper_core.h"

namespace clipperlib {

//------------------------------------------------------------------------------
// Init templates  ...
//------------------------------------------------------------------------------

template struct Point<cInt>;
template struct Point<double>;
template struct Rect<cInt>;
template struct Rect<double>;
template struct Path<cInt>;
template struct Path<double>;
template struct Paths<cInt>;
template struct Paths<double>;
template struct PathsArray<cInt>;
template struct PathsArray<double>;

//------------------------------------------------------------------------------
// Point
//------------------------------------------------------------------------------

void PointI::Rotate(const PointD &center, double angle_rad) {
	double tmp_x = x - center.x;
	double tmp_y = y - center.y;
	double cos_a = cos(angle_rad);
	double sin_a = sin(angle_rad);

	x = (cInt)round(tmp_x * cos_a - tmp_y * sin_a + center.x);
	y = (cInt)round(tmp_x * sin_a - tmp_y * cos_a + center.y);
}
//------------------------------------------------------------------------------

void PointD::Rotate(const PointD &center, double angle_rad) {
	double tmp_x = x - center.x;
	double tmp_y = y - center.y;
	double cos_a = cos(angle_rad);
	double sin_a = sin(angle_rad);

	x = tmp_x * cos_a - tmp_y * sin_a + center.x;
	y = tmp_x * sin_a - tmp_y * cos_a + center.y;
}
//------------------------------------------------------------------------------

void PointI::Rotate(const PointD &center, double sin_a, double cos_a) {
	double tmp_x = x - center.x;
	double tmp_y = y - center.y;

	x = (cInt)round(tmp_x * cos_a - tmp_y * sin_a + center.x);
	y = (cInt)round(tmp_x * sin_a - tmp_y * cos_a + center.y);
}
//------------------------------------------------------------------------------

void PointD::Rotate(const PointD &center, double sin_a, double cos_a) {
	double tmp_x = x - center.x;
	double tmp_y = y - center.y;

	x = tmp_x * cos_a - tmp_y * sin_a + center.x;
	y = tmp_x * sin_a - tmp_y * cos_a + center.y;
}

//------------------------------------------------------------------------------
// Rect
//------------------------------------------------------------------------------

template <typename T>
void Rect<T>::Intersect(const Rect<T> &rect) {
	if (IsEmpty())
		return;
	else if (rect.IsEmpty())
		*this = Rect();
	left = std::max(rect.left, left);
	right = std::min(rect.right, right);
	top = std::max(rect.top, top);
	bottom = std::min(rect.bottom, bottom);
	if (IsEmpty())
		*this = Rect();
}
//------------------------------------------------------------------------------

void RectI::Rotate(double angle_rad) {
	PointD cp;
	PathD pts;

	cp.x = double(right + left) / 2;
	cp.y = double(bottom + top) / 2;

	pts.resize(4);
	pts[0] = PointD((double)left, (double)top);
	pts[1] = PointD((double)right, (double)top);
	pts[2] = PointD((double)right, (double)bottom);
	pts[3] = PointD((double)left, (double)bottom);
	pts.Rotate(cp, angle_rad);

	left = (cInt)floor(pts[0].x);
	top = (cInt)floor(pts[0].y);
	right = (cInt)ceil(pts[0].x);
	bottom = (cInt)ceil(pts[0].y);
	for (size_t i = 1; i < 4; ++i) {
		if (pts[i].x < left) left = (cInt)floor(pts[i].x);
		if (pts[i].y < top) top = (cInt)floor(pts[i].y);
		if (pts[i].x > right) right = (cInt)ceil(pts[i].x);
		if (pts[i].y > bottom) bottom = (cInt)ceil(pts[i].y);
	}
}
//------------------------------------------------------------------------------

void RectD::Rotate(double angle_rad) {
	PointD cp;
	PathD pts;

	cp.x = double(right + left) / 2;
	cp.y = double(bottom + top) / 2;

	pts.resize(4);
	pts[0] = PointD(left, top);
	pts[1] = PointD(right, top);
	pts[2] = PointD(right, bottom);
	pts[3] = PointD(left, bottom);

	pts.Rotate(cp, angle_rad);
	left = pts[0].x;
	top = pts[0].y;
	right = pts[0].x;
	bottom = pts[0].y;

	for (size_t i = 1; i < 4; ++i) {
		if (pts[i].x < left) left = pts[i].x;
		if (pts[i].y < top) top = pts[i].y;
		if (pts[i].x > right) right = pts[i].x;
		if (pts[i].y > bottom) bottom = pts[i].y;
	}
}
//------------------------------------------------------------------------------

template <typename T>
void Rect<T>::Union(const Rect<T> &rect) {
	if (rect.IsEmpty())
		return;
	else if (IsEmpty()) {
		*this = rect;
		return;
	}
	left = std::min(rect.left, left);
	right = std::max(rect.right, right);
	top = std::min(rect.top, top);
	bottom = std::max(rect.bottom, bottom);
}

//------------------------------------------------------------------------------
// Path
//------------------------------------------------------------------------------

PathI::Path(const PathI &other, double scale_x, double scale_y) {
  if (scale_x == 0) scale_x = 1;
  if (scale_y == 0) scale_y = 1;
  if (scale_x == 1 && scale_y == 1) {
    Append(other);
  }
  else {
    data.reserve(other.size());
    std::vector<PointI>::const_iterator it;
    for (it = other.data.begin(); it != other.data.end(); it++)
      data.push_back(PointI((cInt)round(it->x * scale_x), (cInt)round(it->y * scale_y)));
  }
}
//------------------------------------------------------------------------------

PathI::Path(const PathD &other, double scale_x, double scale_y) {
  if (scale_x == 0) scale_x = 1;
  if (scale_y == 0) scale_y = 1;
  data.reserve(other.size());
  std::vector<PointD>::const_iterator it;
  for (it = other.data.begin(); it != other.data.end(); it++)
    data.push_back(PointI((cInt)round(it->x * scale_x), (cInt)round(it->y * scale_y)));
}
//------------------------------------------------------------------------------

PathD::Path(const PathI &other, double scale_x, double scale_y) {
  if (scale_x == 0) scale_x = 1;
  if (scale_y == 0) scale_y = 1;
  data.reserve(other.size());
  std::vector<PointI>::const_iterator it;
  for (it = other.data.begin(); it != other.data.end(); it++)
    data.push_back(PointD(it->x * scale_x, it->y * scale_y));
}
//------------------------------------------------------------------------------

PathD::Path(const PathD &other, double scale_x, double scale_y) {
  if (scale_x == 0) scale_x = 1;
  if (scale_y == 0) scale_y = 1;
  if (scale_x == 1 && scale_y == 1) {
    Append(other);
  }
  else {
    data.reserve(other.size());
    std::vector<PointD>::const_iterator it;
    for (it = other.data.begin(); it != other.data.end(); it++)
      data.push_back(PointD(it->x * scale_x, it->y * scale_y));
  }
}
//------------------------------------------------------------------------------

PathI &PathI::operator=(const PathD &other) {
	data.clear();
	using namespace std;
	vector<PointD>::const_iterator it;
	for (it = other.data.begin(); it != other.data.end(); it++)
		data.push_back(PointI((cInt)round(it->x), (cInt)round(it->y)));
	return *this;
}
//------------------------------------------------------------------------------

PathD &PathD::operator=(const PathI &other) {
	data.clear();
	using namespace std;
	vector<PointI>::const_iterator it;
	for (it = other.data.begin(); it != other.data.end(); it++)
		data.push_back(PointD((double)it->x, (double)it->y));
	return *this;
}
//------------------------------------------------------------------------------

PathI &PathI::operator=(const PathI &other) {
	if (this != &other) {
		data.clear();
		Append(other);
	}
	return *this;
}
//------------------------------------------------------------------------------

PathD &PathD::operator=(const PathD &other) {
	if (this != &other) {
		data.clear();
		Append(other);
	}
	return *this;
}
//------------------------------------------------------------------------------

template <typename T>
void Path<T>::Append(const Path<T> &extra) {
	data.insert(data.end(), extra.data.begin(), extra.data.end());
}
//------------------------------------------------------------------------------

template <typename T>
double Path<T>::Area() const {
	double area = 0.0;
	size_t len = data.size() - 1;
	if (len < 3) return area;
	int j = len - 1;
	for (size_t i = 0; i < len; ++i) {
		double d = (double)(data[j].x + data[i].x);
		area += d * (data[j].y - data[i].y);
		j = i;
	}
	return -area * 0.5;
}
//------------------------------------------------------------------------------

template <typename T>
Rect<T> Path<T>::Bounds() const {
	using namespace std;
	const T _MAX = numeric_limits<T>::max();
	const T _MIN = numeric_limits<T>::min();

	Rect<T> bounds(_MAX, _MAX, _MIN, _MIN);

	typename vector<Point<T> >::const_iterator it;
	for (it = data.begin(); it != data.end(); it++) {
		if (it->x < bounds.left) bounds.left = it->x;
		if (it->x > bounds.right) bounds.right = it->x;
		if (it->y < bounds.top) bounds.top = it->y;
		if (it->y > bounds.bottom) bounds.bottom = it->y;
	}

	if (bounds.left >= bounds.right)
		return Rect<T>();
	else
		return bounds;
}
//------------------------------------------------------------------------------

template <typename T>
void Path<T>::Offset(T dx, T dy) {
	if (dx == 0 && dy == 0) return;
	for (size_t i = 0; i < data.size(); ++i) {
		data[i].x = data[i].x + dx;
		data[i].y = data[i].y + dy;
	}
}
//------------------------------------------------------------------------------

template <typename T>
bool Path<T>::Orientation() const {
	return Area() >= 0;
}
//------------------------------------------------------------------------------

template <typename T>
void Path<T>::Reverse() {
	std::reverse(data.begin(), data.end());
}
//------------------------------------------------------------------------------

template <typename T>
void Path<T>::Rotate(const PointD &center, double angle_rad) {
	double cos_a = cos(angle_rad);
	double sin_a = sin(angle_rad);

	for (size_t i = 0; i < data.size(); ++i)
		data[i].Rotate(center, sin_a, cos_a);
}
//------------------------------------------------------------------------------

template <typename T>
void Path<T>::Scale(T sx, T sy) {
	if (sx == 0) sx = 1;
	if (sy == 0) sy = 1;
	if (sx == 1 && sy == 1) return;

	for (size_t i = 0; i < data.size(); ++i) {
		data[i].x = data[i].x * sx;
		data[i].y = data[i].y * sy;
	}
	StripDuplicates();
}
//------------------------------------------------------------------------------

template <typename T>
void Path<T>::StripDuplicates() {
	using namespace std;
	data.erase(unique(begin(data), end(data)), end(data));
}

//------------------------------------------------------------------------------
// Paths
//------------------------------------------------------------------------------

template <typename T>
Rect<T> Paths<T>::Bounds() const {
	using namespace std;
	const T _MAX = numeric_limits<T>::max();
	const T _MIN = numeric_limits<T>::min();

	Rect<T> bounds(_MAX, _MAX, _MIN, _MIN);

	typename vector<Path<T> >::const_iterator it1;
	typename vector<Point<T> >::const_iterator it2;

	for (it1 = data.begin(); it1 != data.end(); it1++) {
		for (it2 = it1->data.begin(); it2 != it1->data.end(); it2++) {
			if (it2->x < bounds.left) bounds.left = it2->x;
			if (it2->x > bounds.right) bounds.right = it2->x;
			if (it2->y < bounds.top) bounds.top = it2->y;
			if (it2->y > bounds.bottom) bounds.bottom = it2->y;
		}
	}

	if (bounds.left >= bounds.right)
		return Rect<T>();
	else
		return bounds;
}
//------------------------------------------------------------------------------

template <typename T>
void Paths<T>::Rotate(const PointD &center, double angle_rad) {
	double cos_a = cos(angle_rad);
	double sin_a = sin(angle_rad);

	for (size_t i = 0; i < data.size(); ++i)
		for (size_t j = 0; i < data[i].size(); ++j)
			data[i][j].Rotate(center, sin_a, cos_a);
}
//------------------------------------------------------------------------------

template <typename T>
void Paths<T>::Scale(T scale_x, T sy) {
	for (size_t i = 0; i < data.size(); ++i)
		data[i].Scale(scale_x, sy);
}
//------------------------------------------------------------------------------

template <typename T>
void Paths<T>::Offset(T dx, T dy) {
	if (dx == 0 && dy == 0) return;
	for (size_t i = 0; i < data.size(); ++i)
		for (size_t j = 0; j < data[i].size(); ++j) {
			data[i][j].x = data[i][j].x + dx;
			data[i][j].y = data[i][j].y + dy;
		}
}
//------------------------------------------------------------------------------

template <typename T>
void Paths<T>::Reverse() {
	for (size_t i = 0; i < data.size(); ++i)
		data[i].Reverse();
}
//------------------------------------------------------------------------------

template <typename T>
void Paths<T>::Append(const Paths<T> &extra) {
	data.insert(data.end(), extra.data.begin(), extra.data.end());
}

//------------------------------------------------------------------------------
// PathsArray
//------------------------------------------------------------------------------

template <typename T>
Rect<T> PathsArray<T>::Bounds() const {
	using namespace std;
	const T _MAX = numeric_limits<T>::max();
	const T _MIN = numeric_limits<T>::min();

	Rect<T> bounds(_MAX, _MAX, _MIN, _MIN);

	typename vector<Paths<T> >::const_iterator it1;
	typename vector<Path<T> >::const_iterator it2;
	typename vector<Point<T> >::const_iterator it3;

	for (it1 = data.begin(); it1 != data.end(); it1++) {
		for (it2 = it1->data.begin(); it2 != it1->data.end(); it2++) {
			for (it3 = it2->data.begin(); it3 != it2->data.end(); it3++) {
				if (it3->x < bounds.left) bounds.left = it3->x;
				if (it3->x > bounds.right) bounds.right = it3->x;
				if (it3->y < bounds.top) bounds.top = it3->y;
				if (it3->y > bounds.bottom) bounds.bottom = it3->y;
			}
		}
	}

	if (bounds.left >= bounds.right)
		return Rect<T>();
	else
		return bounds;
}

//------------------------------------------------------------------------------
// Miscellaneous
//------------------------------------------------------------------------------

PipResult PointInPolygon(const PointI &pt, const PathI &path) {
	int val = 0;
	size_t cnt = path.size();
	double d, d2, d3;  // using doubles to avoid possible integer overflow
	PointI ip, ip_next;
	PipResult result = pipOnEdge;

	if (cnt < 3) {
		result = pipOutside;
		return result;
	}
	ip = path[0];
	for (size_t i = 1; i < cnt; ++i) {
		if (i < cnt)
			ip_next = path[i];
		else
			ip_next = path[0];

		if (ip_next.y == pt.y) {
			if ((ip_next.x == pt.x) || ((ip.y == pt.y) && ((ip_next.x > pt.x) == (ip.x < pt.x)))) {
				return result;
			}
		}
		if ((ip.y < pt.y) != (ip_next.y < pt.y)) {
			if (ip.x >= pt.x) {
				if (ip_next.x > pt.x) {
					val = 1 - val;
				} else {
					d2 = (double)(ip.x - pt.x);
					d3 = (double)(ip_next.x - pt.x);
					d = d2 * (double)(ip_next.y - pt.y) - d3 * (double)(ip.y - pt.y);
					if (d == 0)
						return result;
					if ((d > 0) == (ip_next.y > ip.y))
						val = 1 - val;
				}
			} else {
				if (ip_next.x > pt.x) {
					d2 = (double)(ip.x - pt.x);
					d3 = (double)(ip_next.x - pt.x);
					d = d2 * (double)(ip_next.y - pt.y) - d3 * (double)(ip.y - pt.y);
					if (d == 0)
						return result;
					if ((d > 0) == (ip_next.y > ip.y))
						val = 1 - val;
				}
			}
		}
		ip = ip_next;
	}
	switch (val) {
		case -1: result = pipOnEdge; break;
		case 1: result = pipInside; break;
		default: result = pipOutside; break;
	}
	return result;
}
//------------------------------------------------------------------------------

double CrossProduct(const PointI &pt1, const PointI &pt2, const PointI &pt3) {
	double x1 = double(pt2.x - pt1.x);
	double y1 = double(pt2.y - pt1.y);
	double x2 = double(pt3.x - pt2.x);
	double y2 = double(pt3.y - pt2.y);
	return (x1 * y2 - y1 * x2);
}
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

}  // namespace clipperlib
