import Base: ==
import Base.show
using DataStructures

mutable struct Metaobject
    value::Vector{Any}
end

mutable struct Slot
    name
    reader
    writer
    initform
    initarg
end

function Base.getindex(instance::Metaobject, index::Int64)
    getfield(instance, :value)[index]
end

function Base.setindex!(instance::Metaobject, value::Any, index::Int64)
    setindex!(getfield(instance, :value), value, index)
end

class_of(instance::Metaobject) = instance[1]

function Base.getproperty(instance::Metaobject, property::Symbol)
    if property == :value
        throw(ArgumentError("The value field should not be accessed directly."))
    else
        instance[1+findall(slot -> slot == property, class_of(instance)[6])[1]]
    end
end

function Base.setproperty!(instance::Metaobject, property::Symbol, value::Any)
    if property == :value
        throw(ArgumentError("The value field should not be accessed directly."))
    else
        instance[1+findall(slot -> slot == property, class_of(instance)[6])[1]] = value
    end
end

function Base.length(instance::Metaobject)
    return length(getfield(instance, :value))
end

class_name(instance::Metaobject) = instance.name
class_direct_slots(instance::Metaobject) = instance.direct_slots
class_slots(instance::Metaobject) = instance.slots
class_direct_superclasses(instance::Metaobject) = instance.direct_superclasses
class_cpl(instance::Metaobject) = instance.cpl
class_direct_methods(instance::Metaobject) = instance.direct_methods

generic_methods(generic_function::Metaobject) = generic_function.methods
method_specializers(method::Metaobject) = method.specializers

isMetaobject(object) = isa(object, Metaobject)

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


_Slot = Metaobject([
    nothing,
    :Slot,
    [],
    [:name, :reader, :writer, :initform, :initarg],
    [],
    [:name, :reader, :writer, :initform, :initarg],
    [],
    []
])

Class = Metaobject([
    nothing, # class_of
    :Class, # name
    [], # direct_superclasses
    [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods], # direct_slots
    [], # cpl
    [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods], # slots
    [], # direct_subclasses
    [] # direct_methods
])

Class[1] = Class
Class[5] = [Class]

Top = Metaobject([
    Class,
    :Top, 
    [], 
    [], 
    [Class], 
    [], 
    [], 
    []
])

Object = Metaobject([
    Class, 
    :Object, 
    [Top], 
    [], 
    [Class], 
    [], 
    [], 
    []
])

Class[3] = [Object]


function parseDirectSlots(direct_slots)
    directSlots = Vector{Slot}()
    for i in 1:length(direct_slots.args)
        # Start by pushing a new slot to the directSlots and filling the slot as we find the information
        push!(directSlots, Slot([nothing for j in 1:5]...))
        slot = direct_slots.args[i]

        # Normal scenario in which no options are given
        if isa(slot, Symbol) directSlots[i].name = slot

        # In case slot options are provided
        elseif isa(slot, Expr)
            # In case name=initarg in one of the slot definitions
            if slot.head == :(=) && length(slot.args == 2)
                directSlots[i].name = slot.args[1]
                directSlots[i].initarg = slot.args[2]

            # In case options are provided [name, reader, writer, initform=initarg] or [name=initarg, reader, writer]
            elseif slot.head == :vect && length(slot.args) > 0 && length(slot.args) <= 4
                for slotDef in slot.args
                    if isa(slotDef, Expr) && slotDef.head == :(=) && length(slotDef.args) == 2
                        if slotDef.args[1] in [:reader, :writer, :initform]
                            setproperty!(directSlots[i], slotDef.args[1], slotDef.args[2])
                        else directSlots[i].initarg = slotDef.args[2] end
                    else directSlots[i].name = slotDef end
                end
            end
        end
    end
    return directSlots
end

macro defclass(name, direct_superclasses, direct_slots)
    parsedDirectSlots = parseDirectSlots(direct_slots)
    return quote
        global $(esc(name)) = $(esc(Metaobject))([
            Class, # class_of
            $(QuoteNode(name)), # name
            vcat($(esc(direct_superclasses)), [$(esc(Object))]), # direct_superclasses
            $(map(x->:($x), direct_slots.args)), # direct_slots
            [$(esc(Class))], # cpl
            $(map(x->:($x), direct_slots.args)), # slots
            [], # direct_subclasses
            [], # direct_methods
        ])
    end
end

@defclass(GenericFunction, [], [name, lambda_list, methods])
@defclass(MultiMethod, [], [lambda_list, specializers, procedure, env, generic_function])


function is_more_specific(method1, method2, args)
    for i in 1:length(method1.specializers)
        index1 = findfirst(x -> x === method1.specializers[i], compute_cpl(class_of(args[i])))
        index2 = findfirst(x -> x === method2.specializers[i], compute_cpl(class_of(args[i])))
        
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
    return all(spec in compute_cpl(class_of(arg)) for (arg,spec) in zip(args, method.specializers))
end

function call_generic_function(generic_function, args...)
    applicable_methods = filter(method->isApplicable(method, args), generic_function.methods)
    if length(applicable_methods) == 0
        no_applicable_method(generic_function, args)
    else 
        specificity_sorted_methods = sort_by_specificity(applicable_methods, args)
        println(["<$(class_name(class_of(method))) $(class_name(method.generic_function))($(join([string(class_name(specializer)) for specializer in method.specializers], ", ")))>" for method in specificity_sorted_methods])
        # TODO: Apply methods  - for now applies only the most specific (implement call_next_method)
        specificity_sorted_methods[1](args...)
    end
end

function function_dispatch(object::Metaobject, args...)
    if class_of(object) === MultiMethod
        object.procedure(args...)
    elseif class_of(object) !== GenericFunction
        throw(error("ERROR: Instances of $(class_name(class_of(object))) are not callable."))
    else call_generic_function(object, args...) end
end

(f::Metaobject)(args...) = function_dispatch(f, args...)

function create_generic_function(name, lambdaList)
    if !isdefined(Main, name)
        quote
            global $(esc(name)) = $(esc(Metaobject))([
                GenericFunction, # class_of
                $(QuoteNode(name)), # name
                $(esc(lambdaList)), # lambda-list
                [], # methods
            ])
        end
    end
end

macro defgeneric(expr)
    name = expr.args[1]
    lambdaList = expr.args[2:end]
    return create_generic_function(name, lambdaList)
end


macro defmethod(expr)
    name = expr.args[1].args[1]
    dump(expr)
    lambda_list = [(isa(_expr, Expr) ? _expr.args[1] : _expr) for _expr in expr.args[1].args[2:end]]
    println(lambda_list)
    specializers = [eval(_expr.args[2]) for _expr in expr.args[1].args[2:end] if isa(_expr, Expr)]
    println(specializers)
    procedure = expr.args[2]

    return quote
        $(create_generic_function(name, lambda_list))

        push!($(esc(name))[4], Metaobject([
            MultiMethod, # class_of
            $((lambda_list...,)), # lambda_list
            $(esc(specializers)), # specializers
            (($(lambda_list...),) -> $procedure), # procedure
            [], # environment
            $(esc(name)) # generic_function
        ]))
    end
end

@defgeneric print_object(object, io)

@defmethod print_object(object::MultiMethod, io) =
    print(io, "<$(class_name(class_of(object))) $(class_name(object.generic_function))($(join([string(class_name(specializer)) for specializer in object.specializers], ", ")))>")

@defmethod print_object(object::GenericFunction, io) =
    print(io, "<$(class_name(class_of(object))) $(class_name(object)) with $(length(generic_methods(object))) $(length(generic_methods(object)) == 1 ? "method" : "methods")>")

@defmethod print_object(object::Class, io) =
    print(io, "<$(class_name(class_of(object))) $(class_name(object))>")

@defmethod print_object(object::Object, io) =
    print(io, "<$(class_name(class_of(object))) $(string(objectid(object), base=62))>")

function Base.show(io::IO, ::MIME"text/plain", metaobject::Metaobject)
    function_dispatch(print_object, metaobject, io)
end

function Base.show(io::IO, slot::Slot)
    show(slot.name)
end

function compute_cpl(instance)
    cpl = []
    supersQueue = Queue{Any}()
    enqueue!(supersQueue, instance)
    while !isempty(supersQueue)
        super = dequeue!(supersQueue)
        for _super in super.direct_superclasses 
            enqueue!(supersQueue, _super) 
        end
        if !(super in cpl)
            push!(cpl, super)
        end
    end
    return cpl
end

function allocate_instance(class)
    return Metaobject([
            class,
            [nothing for slot in class_direct_slots(class)]...
    ])
end

function initialize(instance, initargs)
    for (index, value) in zip(keys(initargs), collect(values(initargs)))
        setproperty!(instance, index, value)
    end
end

new(class; initargs...) = 
    let instance = allocate_instance(class)
        initialize(instance, initargs)
        instance
    end