/**
 * signalobj.d - a few utilities for creating signal objects for more flexible
 *   signals and slots.
 *
 * Written by Bill Baxter, November 9, 2006.
 * This code is released to the public domain
 */
module luigi.signalobj;

// This code triggered an ICE in DMD 0.173.  But now it's fixed in 0.174!!  cool!
import std.stdio : writefln;
import std.signals;
import std.traits;
import std.c.string : memcpy;

//----------------------------------------------------------------------------

/** A simple object that wraps a std.signal.  This allows creating signals at 
 *  global scope, returning them from functions, etc.
 *  In terms of what can be connected, this has the same restrictions as
 *  the std.signal.Signal mixin, namely the slot delegate signature must 
 *  match the signal's signature exactly.
 */
class SignalObj(T...)
{
    alias void delegate(T) slot_t;

    // Methods provided by Signal mixin:
    //   connect(slot_t);
    //   disconnect(slot_t);
    //   emit(T);
    mixin Signal!(T);
}


/**
 * A wrapper for a delegate.  This lets one use delegate literals
 * as a slot in some situations where they would cause access errors.
 */
class DelegateSlot(T...)
{
    alias T args_t;
    alias void delegate(T) func_t;

    this( void delegate(T) dg) { fn = dg; }

    void slot(T args) {
        fn(args);
    }
    func_t fn;
}

/** A simple opaque datatype which serves as a key for FlexSignal.fdisconnect.
 *  The primary use is internal, for keeping a mapping between user slots
 *  and their wrappers.  However, they can also be used as a key to a delegate
 *  literal for later disconnecting.
 */
struct slot_key {
    alias void delegate() slot_t;
    byte[slot_t.sizeof] data=0;
}
slot_key make_slot_key(T)(T fn) {
    alias void delegate() slot_t;
    slot_key f_key;
    static assert(T.sizeof <= slot_t.sizeof);
    memcpy(&f_key, &fn, T.sizeof);
    return f_key;
}

/** FlexSignal is a more flexible version of a signal object wrapper.  
 *
 *  Using the connect/disconnect methods it is possible to connect the 
 *  signal to most any callable entity with a signature that is 'compatible'.
 *  with the signal's signature.
 *
 *  Specifically one can connect 
 *  - functions and delegate literals.
 *  - slots that return values (the return value is simply ignored by the signal)
 *  - slots that take fewer parameters than the signal supplies 
 *    (trailing paramters from signal simply aren't passed to the slot).
 *  - delegates with fewer arguments 
 *  than the signal supplies are allowed, and any argument which can 
 *  be implicitly converted by D is allowed as well.
 */
class FlexSignal(T...)
{
    alias void delegate(T) slot_t;

    private mixin Signal!(T) s;
    slot_t[slot_key] thunkMap;

    /** Emit the signal with the given arguments.
     */
    void emit(T v) {
        s.emit(v);
    }

    /** Connect a slot to the signal using a class delegate exactly 
     *  matching the signature of the signal.
     *  Returns: the slot passed in.
     */
    slot_t connect_exact(slot_t f) {
        s.connect(f);
        return f;
    }

    /** Disconnect a slot from the signal using a class delegate exactly
     *  matching the signature of the signal.
     *  Returns: the slot passed in.
     */
    slot_t disconnect_exact(slot_t f) { 
        s.disconnect(f); 
        return f;
    }

    /** A flexible version of connect.  Works for delegates to classes 
     * or structs, plain functions, delegate literals.  Also works for
     * things with or without return values, and with fewer
     * arguments than the signal supplies.
     *
     * So for example, an (int,char[]) FlexSignal can be connected to an
     * int funciton that returns a float.
     *
     * Also it can convert compatible argument types, any thing that can
     * be implicitly converted at runtime is allowed.
     * 
     * So for example, the (int, char[]) FlexSignal can be connected to a 
     * method taking a (double,char[]) because int is automatically 
     * promoted to double.
     *
     * Returns: a slot key which can be used to disconnect the item
     * later.  (You can also use the original slot to disconnect if you
     * have it.)
     */
    slot_key connect(DT,int arg_offset=0)(DT f)
    {
        // make the key
        static assert(DT.sizeof <= slot_t.sizeof);
        slot_key f_key = make_slot_key(f);
        
        //slot_t f_wrap = SlotAdapter!(slot_t).adapt(f);
        // wrap is safer for now.
        slot_t f_wrap;
        f_wrap = SlotAdapter!(slot_t).wrap!(typeof(f),arg_offset)(f);

        s.connect(f_wrap);

        thunkMap[f_key]=f_wrap;
        return f_key;
    }

    /** Provides an alternate signal for connect() using the ~= operator.
        This can allow for somewhat cleaner syntax when using delegate literals.
        (One set of parentheses is eliminated)
        For example:
            theSignal ~= (Widget w, bool onoff) {
                writefln(onoff?"checked!":"unchecked!");};
        Versus 
            theSignal.connect( (Widget w, bool onoff) {
                writefln(onoff?"checked!":"unchecked!");});
        The downside is that it is not possible to specify template
        arguments for ~= explicitly if needed.
     */
    alias connect opCatAssign;

    /** Some simple wrappers for the most comment skipped argument 
     *  versions of connect.  These work well with implicit instatiation.
     */
    slot_key connect1st(DT)(DT f) { return connect!(DT,0)(f);  }
    slot_key connect2nd(DT)(DT f) { return connect!(DT,1)(f);  }
    slot_key connect3rd(DT)(DT f) { return connect!(DT,2)(f);  }
    slot_key connect4th(DT)(DT f) { return connect!(DT,3)(f);  }
    slot_key connect5th(DT)(DT f) { return connect!(DT,4)(f);  }

    // this doesn't quite work yet because of DMD bug #540
    // Name also has to be different from 'connect' for what also looks 
    // to be a compiler bug.  They should be able to co-exist. 
    // If it did work you'd be able to do something like sig.connecti!(2)(dg).
    template connecti(int arg_offset) {
        slot_key connecti(DT)(DT f)
        {
            return connect!(DT,arg_offset)(f);
        }
    }

    /** Disconnect a slot of any type */
    void disconnect(DT)(DT f) { 
        static if( is(DT==slot_t) ) {
            s.disconnect(f);
        }
        else {
            static if( is(DT==slot_key) ) {
                alias f f_key;
            }
            else {
                // make the key from the delegate
                static assert(DT.sizeof <= slot_t.sizeof);
                slot_key f_key = make_slot_key(f);
            }

            if (f_key in thunkMap) {
                s.disconnect(thunkMap[f_key]);
            } else {
                debug writefln("FlexSignal.fdisconnect: Slot not found");
            }
        }
    }
}

/**
 SlotAdapter is a template that takes a signal's 'slot_t' delegate type.
 It contains two functions, wrap() and adapt().

 wrap() returns a slot_t delegate to object that wraps a passed in
 delegate or function which may have a signature that differs from
 'slot_t'.  As long as the callable entity passed in has argument
 types compatible with 'slot_t' then the operation should succeed.  In
 particular exact matching with slot_t's parameter types is not
 necessary, any type implicitly convertable from the slot_t's argument
 type is ok.

 adapt() is the same as wrap(), except it will return the original
 delegate without creating a wrapper if that delegate's matches the
 slot_t exactly.

 Usage:
   auto wrapped = SlotAdapter!(Signals_SlotType).wrap(target_slot)
   auto adapted = SlotAdapter!(Signals_SlotType).adapt(target_slot)
*/
template SlotAdapter(slot_t)
{
    alias ReturnType!(slot_t) slot_ret_t;
    alias ParameterTypeTuple!(slot_t) slot_arg_t;
    
    static assert( is(slot_ret_t==void), "Expected native slot type to return void."  );
    
    slot_t wrap(WrapSlotT,int arg_offset=0)(WrapSlotT fn) {
        alias WrapSlotT wrap_slot_t;
        alias ReturnType!(fn) wrap_ret_t;
        alias ParameterTypeTuple!(fn) wrap_arg_t;

        // This has to be a _class_ for std.signal's use, currently
        class Inner {
            wrap_slot_t thunked_slot;
            void slot(slot_arg_t arg) 
            {
                thunked_slot(arg[arg_offset..wrap_arg_t.length+arg_offset]);
            }
        }
        Inner inner = new Inner;
        inner.thunked_slot = fn;
        return &inner.slot;
    }

    slot_t adapt(WrapSlotT)(WrapSlotT fn, int arg_offset=0)
    {
        static if (is(slot_t==WrapSlotT)) {
            // This *does* need to be wrapped if this is 
            // a delegate to a struct method, but there's no way to ask D
            // for this information.  Well anyway, disallowing struct
            // is listed as a bug currently.
            //    http://www.digitalmars.com/d/phobos/std_signals.html
            // so there's hope it will change.
            // Use wrap instead of adapt if this is a concern.
            return fn;
        }
        else {
            return wrap(fn,arg_offset);
        }
    }
}

// END LIBRARY CODE

//============================================================================
// Tests below -- not automated: requires eyeball to verify

unittest {
// Object with lots of different slots
class Slotty
{
    char[] name;

    this(char[] name_) {
        name = name_;
    }
    ~this() {
        debug (fsignal) writefln("-- %s.dtor: Kaw, see ya later, Chief!", name);
    }

    void slot0()
    { writefln("-- %s.slot0: got ()", name); }
    void slot1(int i)
    { writefln("-- %s.slot1: got (%s)", name, i); }
    void slot2(int i, char[]s)
    { writefln("-- %s.slot2: got (%s,\"%s\")", name,i,s); }
    void slot3(int i, char[]s, float f)
    { writefln("-- %s.slot3: got (%s, \"%s\", %s)", name,i,s,f); }

    void slot2l(long i, char[]s) { 
        writefln("-- %s.slot2l: got ((long) %s, \"%s\")", name,i,s); 
    }
    void slot2b(byte i, char[]s) { 
        writefln("-- %s.slot2b: got ((byte) %s, \"%s\")", name,i,s); 
    }
    void slot3l(int i, char[]s, long f)
    { writefln("-- %s.slot3l: got (%s, \"%s\", (long) %s", name,i,s,f); }

    // ----------ones with returns--------------
    int slot0r() { 
        writefln("-- %s.slot0r: got () ...  Returning!",name); 
        return 0; 
    }
    int slot1r(int i) { 
        writefln("-- %s.slot1r: got (%s)", name, i," ... Returning!"); 
        return 1; 
    }
    int slot2r(int i, char[]s) { 
        writefln("-- %s.slot2r: got (%s, \"%s\")", name,i,s, " ... Returning!");
        return 2;
    }

    int slot2rl(long i, char[]s) { 
        writefln("-- %s.slot3rl: got (%s, \"%s\")", name,i,s, " ... Returning!");
        return 2;
    }

    int slot3r(int i, char[]s, float f)
    {
        writefln("-- %s.slot3r: got (%s, \"%s\", %s)", name,i,s,f, " ... Returning!");
        return 3;
    }

}

void func0()
{ writefln("-- func0: got()"); }

void func1(int i)
{ writefln("-- func1: got (%s)", i); }

void func2(int i, char[]s)
{ writefln("-- func2: got (%s, \"%s\")",i,s); }

void func3(int i, char[]s, float f)
{ writefln("-- func3: got (%s, \"%s\", %s)",i,s,f); }

double func3ldr(long i, char[]s, double d)
{
    writefln("-- func3ldr: got ((long) %s, \"%s\", (double) %s) ... Returning!",i,s,d);
    return d;
}

void func1s(char [] s)
{ writefln("-- func1s: got(\"%s\")", s); }


void test() {
    auto s1 = new Slotty("Slotty1");
    auto s2 = new Slotty("Slotty2");

    
    auto sig1 = new SignalObj!(int);
    sig1.connect(&s1.slot1);
    //sig1.connect(&s1.slot0); // error mismatch

    writefln(">>> SignalObject EMIT");
    sig1.emit(3);


    writefln("\n>> TEST SlotAdapter...");

    // error as you can't implicitly convert float to long
    //auto test0 =  SlotAdapter!(fsig.slot_t).wrap(&s1.slot3l);
    //test0(9,"tee nine", 9.9);

    auto fsig = new FlexSignal!(int,char[],float);


    alias SlotAdapter!(fsig.slot_t) SAdapt;
    auto test1 =  SAdapt.wrap(&s1.slot2r);
    test1(9,"nine", 9.9);
    auto test2 =  SAdapt.wrap(&s1.slot2);
    test2(9,"nine", 9.9);
    auto test3 =  SAdapt.wrap(&s1.slot3);
    test3(9,"nine", 9.9);


    fsig.connect(&s1.slot3);
    fsig.connect(&s1.slot2);
    fsig.connect(&s1.slot1);
    fsig.connect(&s1.slot0);
    fsig.connect(&s2.slot3);
    fsig.connect(&s2.slot2);
    fsig.connect(&s2.slot1);
    fsig.connect(&s2.slot0);

    fsig.connect(&func0);
    fsig.connect(&func1);
    fsig.connect(&func2);
    fsig.connect(&func3);
    fsig.connect(&func3ldr);

    // Connect, but skip the first arg of the signal
    // I.e. just subscribe to the string part of the message.
    // This IFTI limitation is annoying!
    // fsig.connecti!(1)(&func1s);
    //fsig.connect!(typeof(&func1s),1)(&func1s);
    fsig.connect2nd(&func1s);

    slot_key literal = 
        fsig.connect( (int i){ writefln("@@@ Hello from delegate literal! int=",i); });

    writefln("\n>>> FlexSignal EMIT (3,\"A three\",9.9)");
    fsig.emit(3,"A three!",9.9);

    fsig.disconnect(&s2.slot0);
    fsig.disconnect(&s1.slot3);

    fsig.disconnect(&func0);
    fsig.disconnect(&func1);
    fsig.disconnect(&func2);

    fsig.disconnect(literal);

    writefln("\n>>> FlexSignal EMIT (4,\"A four!\",8.8)");
    fsig.emit(4,"A four!",8.8);

    debug (fsignal) writefln("Program end");
}
test();

} // end unittest

//----------------------------------------------------------------------------
// needed to run tests
version (unittestmain) {
    void main(){}
}

