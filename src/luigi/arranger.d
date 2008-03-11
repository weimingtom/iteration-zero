//---------------------------------------------------------------------
/**
   In Luigi, Arrangers are responsible for deciding how widgets are 
   arranged in a container.  The Arranger interface actually does not
   depend on the GUI portion of the library it all. It only requires
   items to support the Arrangeable interface.  The Arrangeable 
   interface just provides information about an item's preferred, 
   and minimum sizes.

   Note in many toolkits "arranging" is referred to as "layout".
   Layout is a good word, but the problem is that its hard to come up
   with good names for the things that can be laid-out and the thing
   that does the laying-out.  You end up with words like "Layoutable",
   "Layouter", "Layoutee", "Layouting", or "LayoutManager", which are
   all either dubious as English words, or just too long.  Adding to
   the mess is the fact that "layout" is both a verb and a noun, so
   just "layout" alone is ambiguious.  Is layout() a method that does
   the layout or one that returns an arrangement of items, or perhaps
   one that returns the object responsible for laying things out?

   In contrast, starting from the verb "arrange" we can use the very
   reasonable English words "arrange", "arranging", "arranged",
   "arrangement", "arranger", and "arrangeable".

*/ 
//---------------------------------------------------------------------
/*
 Copyright:

  luigi/arranger.d -- arrangers for 'luigi' user interface library.

  Copyright (C) 2006 William V. Baxter III

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  William Baxter wbaxter@gmail.com
*/
module luigi.arranger;

import luigi.base;
import math = std.math;

import std.stdio : writefln;

//--------------------------------------------------------------------------
/** An abstract interface for an Object that can be arranged */
interface Arrangeable
{
    Size minimum_size(Size bounds);
    Size preferred_size(Size bounds);

    /** Return stretch factor in x and y directions.
     *  If component is fixed size, then should be 0.
     *  If nonzero then all nonzero elements share the 
     *  available space in proportion with their stretch 
     *  factors.  So a stretch=2 gets twice as much 
     *  space as a stretch=1.
     */
    int stretch_x();
    int stretch_y();

    void set_rect(Rect s);
    void set_position(Point p);
    void set_size(Size sz);

    /** Arrange any sub-objects */
    void arrange();
}

enum Alignment
{
    Center = 0x44, //CenterX|CenterY

    Left    = 0x01,
    Right   = 0x02,
    CenterX = 0x04,
    FillX   = 0x08,
    Top     = 0x10,
    Bottom  = 0x20,
    CenterY = 0x40,
    FillY   = 0x80,

    Fill = FillX|FillY,

    XMASK = Left|Right|CenterX|FillX,
    YMASK = Top|Bottom|CenterY|FillY,
}

struct Gaps
{
    float x=0,y=0;
    bool standard=true;

    static Gaps opCall(float xgap, float ygap) {
        Gaps g;
        g.standard = false;
        g.x = xgap;
        g.y = ygap;
        return g;
    }

    static Gaps opCall(float gap) {
        Gaps g;
        g.standard = false;
        g.x = g.y = gap;
        return g;
    }

    static Gaps opCall() {
        Gaps g;
        g.standard = true;
        g.x = g.y = 0;
        return g;
    }
}

typedef Gaps Pads;


//--------------------------------------------------------------------------
/** An abstract interface for an Object that arranges things */
interface Arranger
{
    void add(Arrangeable item, ...);
    void remove(Arrangeable item, ...);
    void clear();
    void set_rect(Rect p);

    Size minimum_size(Size bounds);
    Size preferred_size(Size bounds);

    void arrange();

    /// Return the natural order (tab order) of the item.
    /// The item should be contained by the given arranger.
    int orderof(Arrangeable item);

    // ArrangeableIterator child_iterator();

    bool auto_add();
    bool auto_add(bool);
}

//--------------------------------------------------------------------------
/** An abstract base class that implements some common parts of the Arranger 
 *  interface, and leaves others for subclasses to specify.
 */
class BaseArranger : Arranger
{
    this()
    {
        rect = Rect(0,0,0,0);
    }

    // Implementation of arranger interface
    abstract override void add(Arrangeable item,...);
    abstract override void remove(Arrangeable item,...); 
    abstract override void clear();
    abstract override void arrange();

    abstract override Size minimum_size(Size bounds);
    abstract override Size preferred_size(Size bounds);

    abstract override int orderof(Arrangeable a);

    bool auto_add() { return m_auto_add; }
    bool auto_add(bool v) { return m_auto_add=v; }

    override void set_rect(Rect p)
    {
        rect = p;
    }

    void set_size(Size s)
    {
        rect.w = s.w;
        rect.h = s.h;
    }


private:
    Rect rect;
    bool m_auto_add = true;
}

//----------------------------------------------------------------------------
class FlowArranger : BaseArranger
{
    this()
    {
        this(Alignment.Left|Alignment.Top, Gaps());
    }
		
    this(Alignment align_)
    {	
        this(align_, Gaps());
    }

    this(Alignment align_, Gaps gaps_)
    {
        this.rect = Rect(0,0,0,0);
        this.alignment = align_;
        if (gaps_.standard) {
            gaps_.x = DEFAULT_GAP;
            gaps_.y = DEFAULT_GAP;
        }
        m_gaps = gaps_;
    }

    override int orderof(Arrangeable item) {
        assert(item !is null);
        int idx = m_items.find_item(item);
        assert(idx != NOT_FOUND);
        return idx;
    }

    override void add(Arrangeable item, ...)
    in { 
        assert(_arguments.length == 0, "FlowArranger.add does not take arguments");
    }
    body{
        m_items ~= item;
        return item;
    }

    override void remove(Arrangeable item, ...)
    in {
        assert(_arguments.length == 0, "FlowArranger.remove does not take arguments");
    }
    body{
        m_items.drop_item(item);
    }

    override void clear()
    {
        m_items.length = 0;
    }

    Size minimum_size(Size bounds)
    {
        return Size(_max_item_width(), _max_item_height());
    }

    private float _max_item_width()
    {
        float maxWidth = 0;
        foreach(Arrangeable item; m_items)
        {
            maxWidth = math.fmax(maxWidth, item.preferred_size(rect.size).width);
        }
        return maxWidth;
    }

    private float _max_item_height()
    {
        float maxHeight = 0;
        foreach(Arrangeable item ; m_items)
        {
			maxHeight = math.fmax(maxHeight, item.preferred_size(rect.size).height);
	    }
        return maxHeight;
    }

    override  Size preferred_size(Size bounds)
    {
        float htotal=0, height = 0;
        float wtotal = 0, width = 0;
        int rows = 0;
        
        foreach(Arrangeable c; m_items)
		{
            Size s = c.preferred_size(bounds);
            width += s.width + xgap;
            height = max(height, s.height);
            if (width>bounds.width) {
                wtotal = max(wtotal, width-xgap);
                width = 0;
                htotal += height + ygap;
                rows++;
            }
		}
        htotal += height;
        wtotal = max(wtotal,width-xgap);
        return Size(wtotal, htotal);
    }

    override  void arrange()
    {
        FlowRow[] rows;
        float currentWidth = xgap;
        FlowRow currentRow = new FlowRow(xgap);
        foreach(Arrangeable c ; m_items) 
        {
            Size s = c.preferred_size(rect.size);
            currentWidth += s.width + xgap;
            if(currentWidth > rect.width)
            {
                currentWidth = s.width - xgap;
                rows ~= currentRow;
                currentRow = new FlowRow(xgap);
            }
            currentRow.add(c);
        }
        rows ~= currentRow;
        float totalHeight = 0;
        foreach(FlowRow r ; rows)
        {
            totalHeight += r.size.height;
        }
        totalHeight += ygap*(rows.length - 1);

        alias Alignment A;

        float y;
        switch(alignment & A.YMASK) {
        case A.Top:
            y = rect.y;
            break;
        case A.Bottom:
            y = rect.y2 - totalHeight;
            break;
        case A.CenterY:
        default:
            y = rect.y + (rect.height - totalHeight)/2;
            break;

        }

        switch(alignment & A.XMASK)
        {
        case A.Left:
            foreach(FlowRow r; rows) 
            {
                r.position = Point(0, y-rect.y);
                r.arrange();
                y += r.size.height + ygap;
            }
            break;
        case A.Right:
            foreach(FlowRow r; rows)
            {
                float x = rect.x2 - r.size.width;
                r.position = Point(x-rect.x, y-rect.y);
                r.arrange();
                y += r.size.height + ygap;
            }
            break;

        case A.CenterX:
        default:
            foreach(FlowRow r; rows)
            {
                float x = rect.x + (rect.width - r.size.width)/2;
                r.position = Point(x-rect.x, y-rect.y);
                r.arrange();
                y += r.size.height + ygap;
            }
            break;
        }
        foreach(Arrangeable item ; m_items)
        {
            item.arrange();
        }
    }


    float xgap() { return m_gaps.x; }
    float xgap(float v) { m_gaps.x = v; return m_gaps.x; }
    float ygap() { return m_gaps.y; }
    float ygap(float v) { m_gaps.y = v; return m_gaps.y; }

    Alignment alignment() { return m_alignment; }
    Alignment alignment(Alignment v) { m_alignment = v; return m_alignment; }

    const float DEFAULT_GAP = 5;

    private:
    Arrangeable[] m_items;
    Alignment m_alignment;
    Gaps m_gaps;
}

//----------------------------------------------------------------------------
/**
 * FlowRow is a private implementation detail of the FlowArranger class
 */
private class FlowRow
{
    this(float xgap)
    {
        this.xgap = xgap;
    }

    void add(Arrangeable item)
    {
        m_items ~= item;
        _recalc();
        return item;
    }

    void remove(Arrangeable item)
    {
        m_items.drop_item(item);
        _recalc();
    }

    void arrange()
    {
        _recalc();
        float x0 = rect.x;
        float y0 = rect.y;
        float x = x0;
        float y = y0 + (rect.height / 2);
        foreach(Arrangeable item ; m_items)
        {
            Size s = item.preferred_size(rect.size);
            item.set_rect(Rect(x, y-(s.height/2), s.width, s.height));
            x += s.width + xgap;
        }
    }

    private void _recalc()
    {
        float height = _max_item_height();
        float width = 0;
        foreach(Arrangeable item ; m_items)
        {
            Size s = item.preferred_size(rect.size);
            width += s.width;
        }
        width += xgap * (m_items.length - 1);
        m_rect.width = width;
        m_rect.height = height;
    }

    private float _max_item_height()
    {
        float maxHeight = 0;
        foreach(Arrangeable item ; m_items)
        {
            maxHeight = math.fmax(maxHeight, item.preferred_size(rect.size).height);
        }
        return maxHeight;
    }

    float xgap() { return m_xgap; }
    float xgap(float v) { m_xgap = v; _recalc(); return m_xgap; }

    //int ygap() { return m_ygap; }
    //int ygap() { m_ygap = v; _recalc(); return m_ygap; }

    Point position() { return Point(m_rect.x, m_rect.y); }
    Point position(Point p) {
        m_rect.x = p.x;
        m_rect.y = p.y;
        _recalc(); 
        return p; 
    }
    Rect rect() { return m_rect; }
    Rect rect(Rect r) {
        m_rect = r;
        _recalc(); 
        return m_rect;
    }

    Size size() { return Size(m_rect.width, m_rect.height); }

private:
    Arrangeable[] m_items;
    Rect m_rect;
    float m_xgap;
}



//----------------------------------------------------------------------------
/**
 * The GridArranger arranges items into a grid with a fixed number of
 * columns or rows.
 */
class GridArranger : BaseArranger
{
    this()
    {
        this(1, 0, Gaps());
    }

    this(int Columns, int Rows = 0)
    {
        this(Columns, Rows, Gaps());
    }

    this(int Columns, int Rows, Gaps gaps_)
    {
        super();
        this.columns = Columns;
        this.rows = Rows;
        if (gaps_.standard) {
            gaps_.x = 0;
            gaps_.y = 0;
        }
        this.m_gaps = gaps_;
    }

    override int orderof(Arrangeable item) {
        assert(item !is null);
        int idx = m_items.find_item(item);
        assert(idx != NOT_FOUND);
        return idx;
    }

    override Size preferred_size(Size bounds) {
        float[] widths, heights;
        return _calc_size(
            widths,heights,
            (Arrangeable item){ return item.preferred_size(bounds); } );
    }

    override Size minimum_size(Size bounds) {
        float[] widths, heights;
        return _calc_size( 
            widths, heights,
            (Arrangeable item){ return item.minimum_size(bounds); } );
    }

    override void arrange()
    {
        if (m_items.length == 0) return;

        float[] widths, heights;
        GridCells grid = _update_cell_widths(
            widths,heights,
            (Arrangeable i){ return i.preferred_size(rect.size); });


        float x0 = rect.x;
        float y0 = rect.y;
        float x,y;
        int c,r;

        for(c = 0, x = x0; c < grid.cols; x += widths[c] + xgap, c++)
        {
            for(r = 0, y = y0; r < grid.rows; y += heights[r] + ygap, r++)
            {
                int i = r * grid.cols + c;
                if(i < m_items.length)
                {
                    Arrangeable item = m_items[i];
                    item.set_rect(Rect(x-x0, y-y0, widths[c], heights[r]));
                    item.arrange();
                }
            }
        }
    }

    override  void add(Arrangeable item, ...)
    in{
        assert(_arguments.length == 0, "GridArranger.add does not take arguments");
    }
    body{
        m_items ~= item;
        return item;
    }

    override  void remove(Arrangeable item, ...)
    in {
        assert(_arguments.length == 0, "GridArranger.remove does not take arguments");
    }
    body{
        m_items.drop_item(item);
    }

    override  void clear()
    {
        m_items.length = 0;
    }

    float xgap() { return m_gaps.x; }
    float xgap(float v) { m_gaps.x = v; return m_gaps.x; }
    float ygap() { return m_gaps.y; }
    float ygap(float v) { m_gaps.y = v; return m_gaps.y; }


    int rows() { return m_rows; }
    int rows(int v) { return m_rows = v; }

    int columns() { return m_cols; }
    int columns(int v) { return m_cols = v; }

    private struct GridCells {
        static GridCells opCall(int c, int r)
        { GridCells g; g.rows = r; g.cols = c; return g; }
        int cols=0;
        int rows=0;
    }

    private GridCells _calc_grid()
    {
        int newRows = rows;
        int newCols = columns;
        int nItems = m_items.length;
        if(newRows > 0)
        {
            newCols = (nItems + newRows - 1)/newRows;
        }
        else
        {
            newRows = (nItems + newCols - 1)/newCols;
        }
        return GridCells(newCols, newRows);
    }

    private GridCells _update_cell_widths(
        inout float[] widths, inout float[] heights, Size delegate(Arrangeable i)size_getter
        )
    {
        GridCells grid = _calc_grid();
        widths.length = grid.cols;
        heights.length = grid.rows;

        widths[] = 0;
        heights[] = 0;

        foreach(c, inout w; widths) 
        {
            foreach(r, inout h; heights) 
            {
                int i = r * grid.cols + c;
                if(i < m_items.length)
                {
                    Size s = size_getter(m_items[i]);
                    if (s.w > w) w = s.w;
                    if (s.h > h) h = s.h;
                }
            }
        }
        return grid;
    }

    private Size _calc_size(float[] widths, float[]heights, 
                            Size delegate(Arrangeable i) size_getter)
    {
        GridCells grid = _update_cell_widths(widths,heights,size_getter);

        float width = 0;
        float height = 0;
        foreach(h; heights) { height += h; }
        foreach(w; widths) { width += w; }

        return Size(width + xgap * (grid.cols - 1),
                    height + ygap * (grid.rows - 1));
    }
    

    private:
    Arrangeable[] m_items;
    int m_cols,m_rows;
    Gaps m_gaps;
}


//----------------------------------------------------------------------------
/**
 * The BorderArranger can arrange up to five items, which can be
 * either on the north, south, east, or west borders of the parent
 * container, or in the remaining center portion..  The position is specified by 
 * the Region enumerated type.
 */
public class BorderArranger : BaseArranger
{
    enum Region
    {
        North,
        South,
        East,
        West,
        Center
    }

    this() 
    {
        this(Gaps());
    }

    this(Gaps gap_)
    {
        if (gap_.standard) {
            gap_.x = gap_.y = 0;
        }
        m_gaps = gap_;
        auto_add = false;
    }

    override int orderof(Arrangeable item) {
        assert(item !is null);
        if (item==m_north)  { return 0; }
        if (item==m_west)   { return 1; }
        if (item==m_center) { return 2; }
        if (item==m_east)   { return 3; }
        if (item==m_south)  { return 4; }

        assert(0, "Item is not contained in this Arranger.");
        return 0;
    }


    override  Size minimum_size(Size bounds)
    {
        float width = 0;
        float height = 0;
        if(m_north != null){
            Size s = m_north.minimum_size(bounds);
            width = math.fmax(width, s.width);
            height += s.height + ygap;
        }
        if(m_south != null){
            Size s = m_south.minimum_size(bounds);
            width = math.fmax(width, s.width);
            height += s.height + ygap;
        }
        if(m_center != null){
            Size s = m_center.minimum_size(bounds);
            height = math.fmax(height, s.height);
            width += s.width;
        }
        if(m_east != null){
            Size s = m_east.minimum_size(bounds);
            width += s.width;
            height = math.fmax(height, s.height);
        }
        if(m_west != null){
            Size s = m_west.minimum_size(bounds);
            width += s.width;
            height = math.fmax(height, s.height);
        }
        return Size(width, height);
    }

    override  Size preferred_size(Size bounds)
    {
        float width = 0;
        float height = 0;
        if(m_north != null){
            Size s = m_north.preferred_size(bounds);
            width = math.fmax(width, s.width);
            height += s.height + ygap;
        }
        if(m_south != null){
            Size s = m_south.preferred_size(bounds);
            width = math.fmax(width, s.width);
            height += s.height + ygap;
        }
        if(m_center != null){
            Size s = m_center.preferred_size(bounds);
            height = math.fmax(height, s.height);
            width += s.width;
        }
        if(m_east != null){
            Size s = m_east.preferred_size(bounds);
            width += s.width;
            height = math.fmax(height, s.height);
        }
        if(m_west != null){
            Size s = m_west.preferred_size(bounds);
            width += s.width;
            height = math.fmax(height, s.height);
        }
        return Size(width, height);			
    }

    override  void arrange()
    {
        float top = rect.x;
        float left = rect.y;
        float right = rect.x2;
        float bottom = rect.y2;

        if (m_north != null) {
            Size s = m_north.preferred_size(rect.size);
            m_north.set_rect(Rect(left, top, right-left, s.height));
            top += s.height + ygap;
            m_north.arrange();
        }
        if(m_south != null){
            Size s = m_south.preferred_size(rect.size);
            bottom -= s.height;
            m_south.set_rect(Rect(left, bottom, right-left, s.height));
            bottom -= ygap;
            m_south.arrange();
        }
        if(m_west != null){
            Size s = m_west.preferred_size(rect.size);
            m_west.set_rect(Rect(left, top, s.width, bottom - top));
            left += s.width + xgap;
            m_west.arrange();
        }
        if(m_east != null){
            Size s = m_east.preferred_size(rect.size);
            right -= s.width;
            m_east.set_rect(Rect(right, top, s.width, bottom - top));
            right -= xgap;
            m_east.arrange();
        }
        if(m_center != null){
            m_center.set_rect(Rect(left, top, right-left, bottom-top));
            m_center.arrange();
        }
    }

    override  void add(Arrangeable item, ...)
    in {
        assert(_arguments.length <= 1,
               "BorderArranger.add takes one argument, a Region.");
        if (_arguments.length == 1) {
            assert(_arguments[0] == typeid(Region));
        }
    }
    body{
        Region place;
        if (_arguments.length == 1) {
            place = *cast(Region*)_argptr;
        }
        else if (_arguments.length == 0) {
            // Just go in a reasonable order
            if      (!m_north) {  place = Region.North; }
            else if (!m_west)  {  place = Region.West; }
            else if (!m_east)  {  place = Region.East; }
            else if (!m_south) {  place = Region.South; }
            else               {  place = Region.Center; }
        }

        return add(item, place);
    }

    void add(Arrangeable item, Region place)
    {
        switch(place)
        {
        case Region.North:
            m_north = item;
            break;

        case Region.South:
            m_south = item;
            break;

        case Region.East:
            m_east = item;
            break;

        case Region.West:
            m_west = item;
            break;

        case Region.Center:
            m_center = item;
            break;

        default:
            assert(false, "Attempt to add item to non-existing region");
            break;
        }
        return item;
    }

    override  void remove(Arrangeable item, ...)
    in{
        assert(_arguments.length <= 1, "BorderArranger.remove takes one argument, a Region.");
        if (_arguments.length == 1) {
            assert(_arguments[0] == typeid(Region));
        }
    }
    body{
        if (item != null) { 
            if (item == m_north) { m_north = null; }
            if (item == m_south) { m_south = null; }
            if (item == m_east)  { m_east = null; }
            if (item == m_west)  { m_west = null; }
            if (item == m_center) { m_center = null; }
        }
        else if (_arguments[0] == typeid(Region)) 
        {
            Region place = *cast(Region*)_argptr;
            switch(place) {
            case Region.North:  m_north = null; break;
            case Region.South:  m_south = null; break;
            case Region.East:   m_east = null; break;
            case Region.West:   m_west = null; break;
            case Region.Center: m_center = null; break;
            default:
                break;
            }
        }
    }

    override  void clear()
    {
        m_north = m_west = m_east = m_south = m_center = null;
    }

    float xgap() { return m_gaps.x; }
    float xgap(float v) { m_gaps.x = v; return m_gaps.x; }
    float ygap() { return m_gaps.y; }
    float ygap(float v) { m_gaps.y = v; return m_gaps.y; }

private:
    Arrangeable m_north;
    Arrangeable m_south;
    Arrangeable m_east;
    Arrangeable m_west;
    Arrangeable m_center;

    Gaps m_gaps;
}
