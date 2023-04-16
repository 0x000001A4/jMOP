import Base: ==
import Base.show
using DataStructures
import Base: +, -, *, /

#=
All metaobjects in jMOP follow this structure: they consist of a vector that contains the object's fields.
- The first element of the vector corresponds to the class that instantiated this object.
- The remaining elements are the remaining slots of the instantiated object.
=#

mutable struct Metaobject
    value::Vector{Any}
end

function operationCheckMetaobject(o::Metaobject, op::String)
    if class_of(o) !== Slot throw(error("Can only $(op) Metaobjects that are Slots")) end
end

# Define addition
function Base.:+(a::Metaobject, b::Any)
    operationCheckMetaobject(a, "add")
    return a.slotValue + b
end

# Define addition
function Base.:+(a::Metaobject, b::Metaobject)
    operationCheckMetaobject(a, "add")
    return a.slotValue + b.slotValue
end

# Define addition
function Base.:+(a::Metaobject, b::Any)
    operationCheckMetaobject(a, "add")
    return a.slotValue + b
end

function Base.:-(a::Metaobject, b::Metaobject)
    operationCheckMetaobject(a, "sub")
    operationCheckMetaobject(b, "sub")
    return a.slotValue - b.slotValue
end

function Base.:-(a::Metaobject, b::Metaobject)
    operationCheckMetaobject(a, "sub")
    return a.slotValue - b.slotValue
end

function Base.:-(a::Metaobject, b::Any)
    operationCheckMetaobject(a, "sub")
    return a.slotValue - b
end



function Base.:*(a::Metaobject, b::Metaobject)
    operationCheckMetaobject(a, "mul")
    operationCheckMetaobject(b, "mul")
    return a.slotValue * b.slotValue
end

function Base.:*(a::Metaobject, b::Metaobject)
    operationCheckMetaobject(a, "mul")
    return a.slotValue * b.slotValue
end

function Base.:*(a::Metaobject, b::Any)
    operationCheckMetaobject(a, "mul")
    return a.slotValue * b
end


function Base.:/(a::Metaobject, b::Metaobject)
    operationCheckMetaobject(a, "div")
    operationCheckMetaobject(b, "div")
    return a.slotValue / b.slotValue
end

function Base.:-(a::Metaobject, b::Metaobject)
    operationCheckMetaobject(a, "div")
    return a.slotValue / b.slotValue
end

function Base.:-(a::Metaobject, b::Any)
    operationCheckMetaobject(a, "div")
    return a.slotValue / b
end

#=
Note: The value field is an abstraction in order to have the Metaobject julia structure and should not be accessed directly.
This value is accessed directly by indexing the Metaobject instances and for that we specialize the indexing behaviour:
=#

function Base.getindex(instance::Metaobject, index::Int64)
    getfield(instance, :value)[index]
end

function Base.setindex!(instance::Metaobject, value::Any, index::Int64)
    setindex!(getfield(instance, :value), value, index)
end


#=
This is a meta-circular system so, in order to avoid StackOverflows we redefine the behaviour for comparing Metaobjects.
=#

function Base.length(instance::Metaobject)
    return length(getfield(instance, :value))
end

function Base.iterate(instance::Metaobject, state::Int64=1)
    if state > length(instance)
        return nothing
    end
    return (instance[state], state + 1)
end

function compareMetaobjects(metaobject1, metaobject2, compared=Set{Tuple{Metaobject,Metaobject}}())
    if metaobject1 === metaobject2 return true end
    if length(metaobject1) != length(metaobject2) return false end
    if (metaobject1, metaobject2) in compared return true end
    push!(compared, (metaobject1, metaobject2))

    for i in 1:length(metaobject1)
        if isMetaobject(metaobject1[i]) && isMetaobject(metaobject2[i])
            if !compareMetaobjects(metaobject1[i], metaobject2[i], compared)
                return false
            end
        elseif metaobject1[i] != metaobject2[2]
            return false
        end
    end
    return true
end

function ==(metaobject1::Metaobject, metaobject2::Metaobject)
    # In order to avoid infinite recursion
    compareMetaobjects(metaobject1, metaobject2)
end


#=
We test if an instance is a Metaobject with the default julia behaviour, given that all our Metaobjects are an instance of the struct Metaobject (in Julia)
To check what class is this instance an instance of (on this particular object system) we divide it in 2 cases:
-> When in fact the instances are metaobjects they are represented by a vector and its first element has that information.
-> If we are dealing with Julia types then we use the standard typeof() function.
=#

isMetaobject(object) = isa(object, Metaobject)
class_of(instance::Metaobject) = instance[1]


#=#################################################################################################
### Pre-defined Metaobjects:
-> Slot
-> Class
-> Object
-> Top
-> GenericFunction 
-> MultiMethod

#Note: All pre-defined metaobjects are an instance of class so they will have the same structure:
#[Class, name, direct_superclasses, direct_slots, cpl, slots, direct_subclasses, direct_methods]
=#################################################################################################

####################
#       Top        #
####################

Top = Metaobject([
    nothing, # class_of
    :Top, # name
    [], # direct_superclasses
    [], # direct_slots
    [], # cpl
    [], # slots
    [], # direct_subclasses
    [] # direct_methods
])

####################
#       Object     #
####################

Object = Metaobject([
    nothing, # class_of
    :Object, # name
    [Top], # direct_superclasses
    [], # direct_slots
    [], # cpl
    [], # slots
    [], # direct_subclasses
    [] # direct_methods
])


#####################
#  GenericFunction  #
#####################

GenericFunction = Metaobject([
    nothing, # class_of
    :GenericFunction, # name
    [Object], # direct_superclasses
    [], # direct_slots
    [], # cpl
    [], # slots
    [], # direct_subclasses
    [] # direct_methods
])


#####################
#    MultiMethod    #
#####################

MultiMethod = Metaobject([
    nothing, # class_of
    :MultiMethod, # name
    [Object], # direct_superclasses
    [], # direct_slots
    [], # cpl
    [], # slots
    [], # direct_subclasses
    [] # direct_methods
])


####################
#       Slot       #
####################

Slot = Metaobject([
    nothing, # class_of
    :Slot, # name
    [Object], # direct_superclasses
    [], # direct_slots
    [], # cpl
    [], # slots
    [], # direct_subclasses
    [] # direct_methods
])

buildNamedSlot(class, name) = Metaobject([Slot, class, name, missing, missing, missing, missing])


####################
#       Class      #
####################

    
Class = Metaobject([
    nothing, # class_of
    :Class, # name
    [Object], # direct_superclasses
    [], # direct_slots
    [], # cpl
    [], # slots
    [], # direct_subclasses
    [] # direct_methods
])

#=
Create the circular reference:
Class is an instance of all classes in our object system.
=#

Top[1] = Class
Object[1] = Class
Slot[1] = Class
GenericFunction[1] = Class
MultiMethod[1] = Class
Class[1] = Class


Class[4] = [buildNamedSlot(Class, name) for name in [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods]]
Class[6] = [buildNamedSlot(Class, name) for name in [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods]]

Slot[4] = [buildNamedSlot(Class, name) for name in [:class, :name, :reader, :writer, :initform, :slotValue]]
Slot[6] = [buildNamedSlot(Class, name) for name in [:class, :name, :reader, :writer, :initform, :slotValue]]

MultiMethod[4] = [buildNamedSlot(Class, name) for name in [:lambda_list, :specializers, :procedure, :env, :generic_function]]
MultiMethod[6] = [buildNamedSlot(Class, name) for name in [:lambda_list, :specializers, :procedure, :env, :generic_function]]

GenericFunction[4] = [buildNamedSlot(Class, name) for name in [:name, :lambda_list, :methods]]
GenericFunction[6] = [buildNamedSlot(Class, name) for name in [:name, :lambda_list, :methods]]

#=
In order to access or modify the properties of an instance of a Metaobject (without accessing value directly) we also redefine the behaviour to:
- Because instances of slots are metaobjects they all follow the Slot Metaobject structure: 
    [class, name, reader, writer, initform, slotValue] 
    - So in order to access a slot's property, we compare its direct_slots (which are symbols) with any property we are looking for
    - Once we find any property matching that direct_slot we retrieve its index.
    - We then proceed to index the instance with 1+index (given the first element of the instance of a Metaobject is always a class - not in its direct_slots)
- Other metaobjects look in the structure of its class for a slot with the name matching the property and once they find it they follow the same process as above.
=#

isMetaclass(instance) = Class !== instance && Class !== class_of(instance) && Class in class_cpl(class_of(instance))

function Base.getproperty(instance::Metaobject, property::Symbol)
    if property == :value
        throw(ArgumentError("The value field should not be accessed directly."))
    elseif class_of(instance) === Slot
        instance[1+findall(slot -> slot[3] == property, Slot[6])[1]]
    elseif instance === Class && property == :slots
        instance[6]
    elseif isMetaclass(instance) # class_of(instance) is metaclass; instance is not a direct instance of Class
        foundIdx = findall(slot -> slot.name == property, class_of(instance).direct_slots)
        if length(foundIdx) > 0 instance[1+foundIdx[1]]
        else
            foundIdx = findall(slot -> slot.name == property, Class.slots)
            if length(foundIdx) == 0 throw(error("Could not find a field named $(property)")) end
            instance[1+length(class_of(instance).direct_slots)+foundIdx[1]]
        end
    elseif property in [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods]
        foundIdx = findall(slot -> slot.name == property, class_of(instance).slots)
        if length(foundIdx) == 0 throw(error("Could not find a field named $(property)")) end

        instance[1+foundIdx[1]]
    else
        foundIdx = 1
        for _class in class_cpl(class_of(instance) === Class ? instance : class_of(instance))#[2:end]
            direct_slots = _class.direct_slots
            temp = findall(slot -> slot.name == property, direct_slots)
            if length(temp) == 0
                foundIdx += length(direct_slots)
                continue
            end

            return instance[foundIdx + temp[1]]
        end

        throw(error("Could not find a field named $(property)"))
    end
end

function Base.setproperty!(instance::Metaobject, property::Symbol, value::Any)
    if property == :value
        throw(ArgumentError("The value field should not be accessed directly."))
    elseif class_of(instance) === Slot
        instance[1+findall(slot -> slot[3] == property, Slot[6])[1]] = value
    elseif isMetaclass(instance)
        foundIdx = findall(slot -> slot.name == property, class_of(instance).direct_slots)
        if length(foundIdx) > 0 instance[1+foundIdx[1]] = value
        else
            foundIdx = findall(slot -> slot.name == property, Class.slots)
            if length(foundIdx) == 0 throw(error("Could not find a field named $(property)")) end
            instance[1+length(class_of(instance).direct_slots)+foundIdx[1]] = value
        end
    elseif property in [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods]
        foundIdx = findall(slot -> slot.name == property, class_of(instance).slots)
        if length(foundIdx) == 0 throw(error("Could not find a field named $(property)")) end

        instance[1+foundIdx[1]] = value
    else
        foundIdx = 1
        for _class in class_cpl(class_of(instance) === Class ? instance : class_of(instance))#[2:end]
            direct_slots = _class.direct_slots
            temp = findall(slot -> slot.name == property, direct_slots)
            if length(temp) == 0
                foundIdx += length(direct_slots)
                continue
            end

            instance[foundIdx + temp[1]] = value
            return
        end

        throw(error("Could not find a field named $(property)"))
    end
end


#=
Functions in order to introspect instances of Metaobject.
=#

class_name(instance::Metaobject) = instance.name
class_direct_slots(instance::Metaobject) = instance.direct_slots
class_slots(instance::Metaobject) = instance.slots
class_direct_superclasses(instance::Metaobject) = instance.direct_superclasses
class_cpl(instance::Metaobject) = instance.cpl
class_direct_methods(instance::Metaobject) = instance.direct_methods

generic_methods(generic_function::Metaobject) = generic_function.methods
method_specializers(method::Metaobject) = method.specializers


function compute_cpl(instance)
    # println("\n\n@ new compute cpl\n")
    ObjectInSupers = false
    cpl = []
    supersQueue = Queue{Metaobject}()
    enqueue!(supersQueue, instance)
    while !isempty(supersQueue)
        super = dequeue!(supersQueue)
        # println(class_name(super))
        for _super in super.direct_superclasses
            if isMetaobject(_super) enqueue!(supersQueue, _super) end
        end
        if !(super in cpl)
            if super === Object ObjectInSupers = true
            elseif super !== Top push!(cpl, super) end
        end
    end
    append!(cpl, ObjectInSupers ? [Object, Top] : [Top])
    return cpl
end

std_compute_slots(instance::Metaobject) = vcat(map(class -> class_direct_slots(class), class_cpl(instance))...)
map(object -> object.cpl = compute_cpl(object), [Top, Object, Slot, Class, GenericFunction, MultiMethod])


function parseDirectSlots(direct_slots, metaclass)
    directSlots = Vector{Metaobject}()
    for i in 1:length(direct_slots.args)
        # Start by pushing a new slot to the directSlots and filling the slot as we find the information
        slot = direct_slots.args[i]
        push!(directSlots, buildNamedSlot(Class, missing))

        # Normal scenario in which no options are given
        if isa(slot, Symbol) 
            directSlots[i].name = slot

        # In case slot options are provided
        elseif isa(slot, Expr)
            # In case name=initarg in one of the slot definitions
            if slot.head == :(=) && length(slot.args) == 2
                directSlots[i].name = slot.args[1]
                directSlots[i].initform = slot.args[2]
                directSlots[i].slotValue = slot.args[2]

            # In case options are provided [name, reader, writer, initform=initarg] or [name=initarg, reader, writer]
            elseif slot.head == :vect && length(slot.args) > 0 && length(slot.args) <= 4
                for slotDef in slot.args
                    if isa(slotDef, Expr) && slotDef.head == :(=) && length(slotDef.args) == 2
                        if slotDef.args[1] in [:reader, :writer, :initform]
                            setproperty!(directSlots[i], slotDef.args[1], slotDef.args[2])
                        else 
                            directSlots[i].name = slotDef.args[1]
                            directSlots[i].initform = slot.args[2]
                            directSlots[i].slotValue = slotDef.args[2]
                        end
                    else directSlots[i].name = slotDef end
                end
            end
        end
    end
    return directSlots
end

function compute_slot_reader_expr(method, spec, field)
    quote 
        $(method)(o::$(spec)) = o.$(field)
    end
end

function compute_slot_writer_expr(method, spec, field)
    quote 
        $(method)(o::$(spec), v) = o.$(field) = v
    end
end

function create_generic_function(name, lambdaList)
    if !isdefined(Main, name)
        quote
            global $(name) = Metaobject([
                GenericFunction, # class_of
                $(QuoteNode(name)), # name
                $(lambdaList), # lambda-list
                [], # methods
            ])
        end
    end    
end

macro defgeneric(expr)
    name = expr.args[1]
    lambdaList = expr.args[2:end]
    create_generic_function(name, lambdaList)
end

function create_method(expr)
    name = expr.args[1].args[1]
    lambda_list = [(isa(_expr, Expr) ? _expr.args[1] : _expr) for _expr in expr.args[1].args[2:end]]
    specializers = [_expr.args[2] for _expr in expr.args[1].args[2:end] if isa(_expr, Expr)]
    procedure = expr.args[2]
    quote
        $(create_generic_function(name, lambda_list))
        push!($(name)[4], Metaobject([
            MultiMethod, # class_of
            $((lambda_list...,)), # lambda_list
            [$(specializers...)], # specializers
            (($(vcat(lambda_list, [:call_next_method, :i])...),) -> $procedure), # procedure
            [], # environment
            $(name) # generic_function
        ]))
    end
end

macro defmethod(expr)
    create_method(expr)
end

function defineClassOptionsMethods(parsedDirectSlots, spec)
    readerSlots = [slot for slot in parsedDirectSlots if !ismissing(slot.reader)]
    writerSlots = [slot for slot in parsedDirectSlots if !ismissing(slot.writer)]
    reader_methods = [create_method(compute_slot_reader_expr(slot.reader, spec, slot.name).args[2]) for slot in readerSlots]
    writer_methods = [create_method(compute_slot_writer_expr(slot.writer, spec, slot.name).args[2]) for slot in writerSlots]
    all_methods = vcat(reader_methods, writer_methods)
    quote
        $(all_methods...)
    end
end

function parseKwargs(kwargs)
    for expr in kwargs
        if expr.args[1] == :metaclass
            return expr.args[2]
        end
    end
    return nothing
end

function parseDirectSuperclasses(direct_superclasses)
    quote
        if length($direct_superclasses) == 0
            [vcat($(direct_superclasses), [Object])...]
        else
            all(super -> isMetaobject(super), $(direct_superclasses)) ? 
                $(direct_superclasses) :
                [vcat($(direct_superclasses), [Top])...]
        end
    end
end

compute_class_cpl(instance) = quote
    $(instance).cpl = compute_cpl($instance)
end

compute_class_slots(instance) = quote
    $(instance).slots = std_compute_slots($instance)
end

copySlotToInstance(slot::Metaobject) = Metaobject([Slot, Object, slot.name, slot.reader, slot.writer, slot.initform, slot.slotValue])

@defmethod allocate_instance(class::Class) = 
    Metaobject([
        class,
        map(copySlotToInstance, vcat(class_slots(class), class_direct_slots(class)))...,
        #vcat([map(copySlotToInstance, class_direct_slots(_class)) for _class in class_cpl(class)[end:-1:1]]...)...
        #[map(copySlotToInstance, class_direct_slots(_class)) for _class in class_cpl(class)[end:-1:1]]...
])

@defmethod initialize(instance::Object, initargs) = begin
    class = class_of(instance)
    for slot in class.slots
        if !ismissing(slot.initform)
            setproperty!(instance, slot.name, slot.initform)
        end
    end
    for (index, value) in zip(keys(initargs), collect(values(initargs)))
        setproperty!(instance, index, value)
    end
end

@defmethod initialize(instance::Class, initargs) = call_next_method()
@defmethod initialize(generic::GenericFunction, initargs) = call_next_method()
@defmethod initialize(method::MultiMethod, initargs) = call_next_method()

new(class; initargs...) = 
    let instance = allocate_instance(class)
        initialize(instance, initargs)
        instance
    end

    
function is_more_specific(method1, method2, args)
    for i in 1:length(method1.specializers)
        index1 = findfirst(x -> x === method1.specializers[i], class_cpl(class_of(args[i])))
        index2 = findfirst(x -> x === method2.specializers[i], class_cpl(class_of(args[i])))
        
        if index1 < index2
            return true
        elseif index1 > index2
            return false
        end
    end
    return false
end


function sort_by_specificity(methods, args)
    sort!(methods, lt = (x, y) -> is_more_specific(x, y, args))
    return methods
end

function no_applicable_method(generic_function, args)
    throw(error("ERROR:  No applicable method for function $(class_name(generic_function)) with arguments $(join([string((isMetaobject(arg) ? class_name(class_of(arg)) : arg)) for arg in args], ", "))"))
end

function isApplicable(method, args) 
    return all(spec in class_cpl(class_of(arg)) for (arg,spec) in zip(args, method.specializers))
end

function call_method(method, args...)
    method.procedure(args...)
end


function call_generic_function(generic_function::Metaobject, args...)
    if class_of(generic_function) !== GenericFunction
        throw(error("ERROR: Instances of $(class_name(class_of(generic_function))) are not callable."))
    else
        # Find applicable methods
        applicable_methods = filter(method->isApplicable(method, args), generic_function.methods)
        if length(applicable_methods) == 0 
            no_applicable_method(generic_function, args)
        else
            specializedMethods = sort_by_specificity(applicable_methods, args)
            i = 2
            function call_next_method()
                call_method(specializedMethods[i], args..., call_next_method, i+1)
            end
            call_method(specializedMethods[1], args..., call_next_method, 2)
        end
    end
end

(f::Metaobject)(args...) = call_generic_function(f, args...)

@defgeneric print_object(object, io)

@defmethod print_object(object::Slot, io) =
    print(io, (object.class === Class ? ":$(object.name)" : object.slotValue))

@defmethod print_object(object::MultiMethod, io) =
    print(io, "<$(class_name(class_of(object))) $(class_name(object.generic_function))($(join([string(class_name(specializer)) for specializer in object.specializers], ", ")))>")

@defmethod print_object(object::GenericFunction, io) =
    print(io, "<$(class_name(class_of(object))) $(class_name(object)) with $(length(generic_methods(object))) $(length(generic_methods(object)) == 1 ? "method" : "methods")>")

@defmethod print_object(object::Class, io) =
    print(io, "<$(class_name(class_of(object))) $(class_name(object))>")

@defmethod print_object(object::Object, io) =
    print(io, "<$(class_name(class_of(object))) $(string(objectid(object), base=62))>")

function Base.show(io::IO, ::MIME"text/plain", metaobject::Metaobject)
    call_generic_function(print_object, metaobject, io)
end



macro defclass(name, direct_superclasses, direct_slots, kwargs...)
    metaclass = parseKwargs(kwargs)
    parsedDirectSlots = parseDirectSlots(direct_slots, metaclass)
    metaclassSlots = !isnothing(metaclass) ? 
        quote [copySlotToInstance(slot) for slot in class_direct_slots($metaclass)] end :
        quote [] end
    quote
        global $(esc(name)) = Metaobject([
            $(isnothing(metaclass) ? Class : metaclass), # class_of
            $(metaclassSlots)...,
            $(QuoteNode(name)), # name
            $(parseDirectSuperclasses(direct_superclasses)), # direct_superclasses
            $(parsedDirectSlots), # direct_slots
            [], # cpl
            [], # slots
            [], # direct_subclasses
            [], # direct_methods
        ])
        $(compute_class_cpl(name))
        $(defineClassOptionsMethods(parsedDirectSlots, name))
        $(compute_class_slots(name))
    end
end

@defclass(BuiltInClass, [Class], [])

@defclass(_String, [String], [], metaclass=BuiltInClass)
@defclass(_Bool, [Bool], [], metaclass=BuiltInClass)
@defclass(_Float64, [Float64], [], metaclass=BuiltInClass)
@defclass(_Int64, [Int64], [], metaclass=BuiltInClass)

class_of(instance::String) = _String
class_of(instance::Int64) = _Int64
class_of(instance::Bool) = _Bool
class_of(instance::Float64) = _Float64



#-#-#-#-#--#-#-#-#-#-#
#       TESTS        #
#-#-#-#-#--#-#-#-#-#-#

@defclass(Shape, [], [])
@defclass(Device, [], [])

@defclass(Line, [Shape], [from, to])
@defclass(Circle, [Shape], [center, radius])

@defclass(Screen, [Device], [])
@defclass(Printer, [Device], [])

@defgeneric draw(shape, device)

@defmethod draw(shape::Line, device::Screen) = println("Drawing a Line on Screen")
@defmethod draw(shape::Circle, device::Screen) = println("Drawing a Circle on Screen")
@defmethod draw(shape::Line, device::Printer) = println("Drawing a Line on Printer")
@defmethod draw(shape::Circle, device::Printer) = println("Drawing a Circle on Printer")

@defclass(ColorMixin, [],
    [[color, reader=get_color, writer=set_color!]])

@defmethod draw(s::ColorMixin, d::Device) =
    let previous_color = get_device_color(d)
        set_device_color!(d, get_color(s))
        call_next_method()
        set_device_color!(d, previous_color)
    end

@defclass(ColoredLine, [ColorMixin, Line], [])
@defclass(ColoredCircle, [ColorMixin, Circle], [])

@defclass(ColoredPrinter, [Printer],
    [[ink=:black, reader=get_device_color, writer=_set_device_color!]])

@defmethod set_device_color!(d::ColoredPrinter, color) = begin
    println("Changing printer ink color to $color")
    _set_device_color!(d, color)
end

let shapes = [new(Line), new(ColoredCircle, color=:red), new(ColoredLine, color=:blue)],
    printer = new(ColoredPrinter, ink=:black)

    for shape in shapes
        # println("_/ Drawing $(class_of(shape).name)... \\_")
        draw(shape, printer)
        # println("\\_ $(class_of(shape).name) drawn. _/")
    end
end

@defclass(A, [], [])
@defclass(B, [], [])
@defclass(C, [], [])
@defclass(D, [A, B], [])
@defclass(E, [A, C], [])
@defclass(F, [D, E], [])

compute_cpl(F)

@defclass(CountingClass, [Class],
    [counter=0])

@defmethod allocate_instance(class::CountingClass) = begin
        class.counter += 1
        call_next_method()
    end

@defclass(CountingFoo, [], [], metaclass=CountingClass)
@defclass(CountingBar, [], [], metaclass=CountingClass)

new(CountingFoo)
new(CountingFoo)
new(CountingBar)
CountingFoo.counter
CountingBar.counter
if CountingFoo.counter != 2 throw("CountingFoo counter is $(CountingFoo.counter), it should be 2 >:(") end
if CountingBar.counter != 1 throw("CountingBar counter is $(CountingBar.counter), it should be 1 >:(") end

@defmethod compute_slots(instance::Class) = vcat(map(class -> class_direct_slots(class), class_cpl(instance))...)

@defclass(Foo, [], [a=1, b=2])
@defclass(Bar, [], [b=3, c=4])
@defclass(FooBar, [Foo, Bar], [a=5, d=6])

class_slots(FooBar)

foobar1 = new(FooBar)
println(foobar1.a)
println(foobar1.b)
println(foobar1.c)
println(foobar1.d)

@defclass(AvoidCollisionsClass, [Class], [])

@defmethod compute_slots(class::AvoidCollisionsClass) =
    let slots = call_next_method(),
        duplicates = symdiff(slots, unique(slots))
        isempty(duplicates) ? slots :
        error("Multiple occurrences of slots: $(join(map(string, duplicates), ", "))")
    end

@defclass(FooBar2, [Foo, Bar], [a=5, d=6], metaclass=AvoidCollisionsClass)
