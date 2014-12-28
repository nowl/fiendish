#include <sys/time.h>
#include <cstdio>
#include <deque>
#include <vector>
#include <memory>
#include <unordered_map>
#include <sstream>

#define WIDTH 1200
#define HEIGHT 380

using namespace std;

struct Point {
	int x, y;
	int val;
};

struct MetaVal {
	virtual ~MetaVal() {}
};

template <typename T>
struct PolyMetaVal : public MetaVal {
	T val;
};

enum class ObjectType {
	NAME = 0,
		NUM,
		TOTAL
};

struct MapCell;

struct Object {
	MapCell *cell;

	bool types[static_cast<int>(ObjectType::TOTAL)];

	string name;
	int num;
	/*
	unordered_map<string, unique_ptr<MetaVal>> meta;

	template <typename T>
	T& getMetaVal(const string& str) {
		return (static_cast<PolyMetaVal<T>*>(meta[str].get()))->val;
	}

	template <typename T>
	void setMetaVal(const string& str, const T& val) {
		auto metaval = unique_ptr<MetaVal>(new PolyMetaVal<T>());
		(static_cast<PolyMetaVal<T>*>(metaval.get()))->val = val;
		meta[str] = move(metaval);
	}
	*/

	Object() {
		for (int i=0; i<static_cast<int>(ObjectType::TOTAL); i++)
			types[i] = false;
	}

	bool add_type(const ObjectType& type) {
		types[static_cast<int>(type)] = true;
	}

	bool has_type(const ObjectType& type) const {
		return types[static_cast<int>(type)];
	}
};

static vector<unique_ptr<Object>> objects;
Object *object_factory() {
	auto o = unique_ptr<Object>(new Object());
	Object *op = o.get();
	objects.push_back(move(o));
	return op;
}

struct MapCell {
	int val;
	vector<Object*> objs;
};

class DijkstraMapInterface {
public:
	virtual ~DijkstraMapInterface() {};
	virtual vector<Point> getAdjacencies(const Point& p) const = 0;
	virtual void setPoint(const Point& p) = 0;
};

class Map {
public:
	Map(int width, int height)
		: mWidth(width), mHeight(height), mMap(width*height)
	{}

	MapCell *getCell(const Point& p) {
		if (p.x < 0 || p.x >= mWidth || p.y < 0 || p.y >= mHeight)
			return nullptr;
		else
			return &mMap[mWidth*p.y + p.x];
	}

	vector<Object*> objs_around(const Point& po, int dist) {
		vector<Object*> objs;
		Point p;
		for(int y=po.y-dist; y<po.y+dist; y++) { 
			for(int x=po.x-dist; x<po.x+dist; x++) {
				p.x = x; p.y = y;
				auto cell = getCell(p);
				if(cell) {
					auto o = cell->objs.begin();
					for(; o != cell->objs.end(); o++)
						objs.push_back(*o);
				}
			}
		}
		return objs;
	}

private:
	int mWidth, mHeight;
	vector<MapCell> mMap;
};

class DijkstraMap : public DijkstraMapInterface {
public:
	DijkstraMap(Map& map)
		: mMap(map)
	{}

	~DijkstraMap() {}

	vector<Point> getAdjacencies(const Point& p) const {
		vector<Point> points;
		
		Point pt = {p.x-1, p.y};
		if (mMap.getCell(pt)->val == 0)
			points.push_back(pt);
		pt = {p.x+1, p.y};
		if (mMap.getCell(pt)->val == 0)
			points.push_back(pt);
		pt = {p.x, p.y-1};
		if (mMap.getCell(pt)->val == 0)
			points.push_back(pt);
		pt = {p.x, p.y+1};
		if (mMap.getCell(pt)->val == 0)
			points.push_back(pt);

		return points;
	}

	void setPoint(const Point& p) {
		mMap.getCell(p)->val = p.val;
	}

private:
	Map& mMap;
};

void run_dijkstra_map(DijkstraMapInterface& mint, const vector<Point>& goals) {
	deque<Point> queue;	

	for (auto iter=goals.begin(); iter != goals.end(); iter++) {
		mint.setPoint(*iter);
		queue.push_back(*iter);
	}

	while (!queue.empty()) {
		Point p = queue.front();
		queue.pop_front();

		vector<Point> adj = mint.getAdjacencies(p);
		for (auto iter=adj.begin(); iter != adj.end(); iter++) {
			Point adjPoint = *iter;
			if (adjPoint.val > p.val+1 && adjPoint.val != 255) {
				adjPoint.val = p.val+1;
				mint.setPoint(adjPoint);
				queue.push_back(adjPoint);
			}
		}
	}
}

int main (int argc, char *argv[]) {
	struct timeval tv_start, tv;

	//gettimeofday(&tv_start, NULL);

	Map map(WIDTH, HEIGHT);
	for (int i=0; i<WIDTH*HEIGHT; i++) {
		auto cell = map.getCell(Point{i%WIDTH, i/WIDTH, 0});
		cell->val = 200;
		auto obj = object_factory();
		obj->cell = cell;
		//obj->setMetaVal("testing", rand()%100);
		obj->add_type(ObjectType::NUM);
		obj->num = rand()%100;
		cell->objs.push_back(obj);
		obj = object_factory();
		obj->cell = cell;
		//stringstream ss;
		//ss << "hello" << rand()%100;
		//obj->setMetaVal("testing2", ss.str());
		//obj->add_type(ObjectType::NAME);
		//obj->name = "hello";//ss.str();
		obj->add_type(ObjectType::NUM);
		obj->num = rand()%100;
		cell->objs.push_back(obj);
	}

	gettimeofday(&tv_start, NULL);

	DijkstraMap mint(map);

	vector<Point> goals = {{5, 5, 0}, {6,6,0},
						   {6, 5, 255}, {7, 5, 255}, {8, 5, 255},
						   {6, 7, 255}, {7, 7, 255}, {8, 7, 255},
						   {5, 6, 255}, {5, 7, 255}, {5, 8, 255}};
	run_dijkstra_map(mint, goals);

	//print_map();

	printf("around = %d\n", (int)map.objs_around(Point{10, 10}, 3).size());
	printf("around = %d\n", (int)map.objs_around(Point{0, 0}, 3).size());

	auto objs_ar = map.objs_around(Point{10,20}, 2);
	for(auto iter = objs_ar.begin(); iter != objs_ar.end(); iter++) {
		Object *op = *iter;
		/*
		if(op->meta.count("testing2")) {
			printf("%s\n", op->getMetaVal<string>("testing2").c_str());
		} else if(op->meta.count("testing")) {
			printf("%d\n", op->getMetaVal<int>("testing"));
		} else {
			printf("no key\n");
		}
		*/
		if(op->has_type(ObjectType::NAME)) {
			printf("%s\n", op->name.c_str());
		} else if (op->has_type(ObjectType::NUM)) {
			printf("%d\n", op->num);
		} else {
			printf("no key\n");
		}
	}

	gettimeofday(&tv, NULL);

	printf("duration %f\n", (tv.tv_sec - tv_start.tv_sec) * 1e6 + 
		   (tv.tv_usec - tv_start.tv_usec));

	printf("created %d objects\n", objects.size());
	
	return 0;
}
