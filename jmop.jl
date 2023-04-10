import Base: ==
import Base.show
using DataStructures


#=
All metaobjects in jMOP follow this structure: they consist of a vector that contains the object's fields.
- The first element of the vector corresponds to the class that instantiated this object.
- The remaining elements are the remaining slots of the instantiated object.
=#

mutable struct Metaobject
    value::Vector{Any}
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
class_of(instance) = isMetaobject(instance) ? instance[1] : eval(quote $(Symbol("_", typeof(instance))) end)


#=#################################################################################################
### Pre-defined Metaobjects:
-> Slot
-> Class
-> Object
-> Top
-> GenericFunction 
-> MultiMethod

Note: All pre-defined metaobjects are an instance of class so they will have the same structure:
[Class, name, direct_superclasses, direct_slots, cpl, slots, direct_subclasses, direct_methods]
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
    [:name, :lambda_list, :methods], # direct_slots
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
    :GenericFunction, # name
    [Object], # direct_superclasses
    [:lambda_list, :specializers, :procedure, :env, :generic_function], # direct_slots
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
    [:class, :name, :reader, :writer, :initform, :slotValue], # direct_slots
    [], # cpl
    [], # slots
    [], # direct_subclasses
    [] # direct_methods
])

buildNamedSlot(class, name) = Metaobject([Slot, class, name, missing, missing, missing, missing])
Slot[6] = [buildNamedSlot(Slot, name) for name in [:class, :name, :reader, :writer, :initform, :slotValue]]


####################
#       Class      #
####################

    
Class = Metaobject([
    nothing, # class_of
    :Class, # name
    [Object], # direct_superclasses
    [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods], # direct_slots
    [], # cpl
    [], # slots
    [], # direct_subclasses
    [] # direct_methods
])

Class[6] = [buildNamedSlot(Class, _name) for _name in [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods]]

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



#=
In order to access or modify the properties of an instance of a Metaobject (without accessing value directly) we also redefine the behaviour to:
- Because instances of slots are metaobjects they all follow the Slot Metaobject structure: 
    [class, name, reader, writer, initform, slotValue] 
    - So in order to access a slot's property, we compare its direct_slots (which are symbols) with any property we are looking for
    - Once we find any property matching that direct_slot we retrieve its index.
    - We then proceed to index the instance with 1+index (given the first element of the instance of a Metaobject is always a class - not in its direct_slots)
- Other metaobjects look in the structure of its class for a slot with the name matching the property and once they find it they follow the same process as above.
=#

function Base.getproperty(instance::Metaobject, property::Symbol)
    if property == :value
        throw(ArgumentError("The value field should not be accessed directly."))
    elseif class_of(instance) === Slot
        instance[1+findall(slotSymbol -> slotSymbol == property, Slot[4])[1]] # In case we are trying to access a slot of a Slot (ex. getting its name) retrieve the position from direct_superclasses
    else
        if class_of(instance) === Class && property == :slots instance[6]
        else
            foundIdx = findall(slot -> slot.name == property, class_of(instance).slots)
            if length(foundIdx) == 0 throw(error("Could not find a field named $(property)")) end
            instance[1+foundIdx[1]]
        end
    end
end

function Base.setproperty!(instance::Metaobject, property::Symbol, value::Any)
    if property == :value
        throw(ArgumentError("The value field should not be accessed directly."))
    else
        instance[1+findall(slot -> slot.name == property, class_of(instance)[6])[1]] = value
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
    ObjectInSupers = false
    cpl = []
    supersQueue = Queue{Any}()
    enqueue!(supersQueue, instance)
    while !isempty(supersQueue)
        super = dequeue!(supersQueue)
        for _super in super.direct_superclasses
            enqueue!(supersQueue, _super) 
        end
        if !(super in cpl)
            if super === Object ObjectInSupers = true
            elseif super !== Top push!(cpl, super) end
        end
    end
    append!(cpl, ObjectInSupers ? [Object, Top] : [Top])
    return cpl
end

compute_slots(instance) = vcat(map(class -> [buildNamedSlot(class, slotName) for slotName in class_direct_slots(class)], class_cpl(instance))...)

map(object -> object.cpl = compute_cpl(object), [Top, Object, Slot, Class, GenericFunction, MultiMethod])
map(object -> object.slots = compute_slots(object), [Top, Object, Slot, Class, GenericFunction, MultiMethod])



function compute_inherited_slots(object, direct_superclasses, slots=[])
    if length(direct_superclasses) == 0
        return slots
    end
    superclass = popfirst!(direct_superclasses)
    if isMetaobject(superclass) append!(slots, class_slots(superclass)) end
    compute_inherited_slots(object, direct_superclasses, slots)
end


function parseDirectSlots(direct_slots, metaclass)
    directSlots = Vector{Metaobject}()
    for i in 1:length(direct_slots.args)
        # Start by pushing a new slot to the directSlots and filling the slot as we find the information
        slot = direct_slots.args[i]
        push!(directSlots, buildNamedSlot(metaclass === nothing ? Class : metaclass, missing))

        # Normal scenario in which no options are given
        if isa(slot, Symbol) 
            directSlots[i].name = slot

        # In case slot options are provided
        elseif isa(slot, Expr)
            # In case name=initarg in one of the slot definitions
            if slot.head == :(=) && length(slot.args) == 2
                directSlots[i].name = slot.args[1]
                directSlots[i].slotValue = slot.args[2]

            # In case options are provided [name, reader, writer, initform=initarg] or [name=initarg, reader, writer]
            elseif slot.head == :vect && length(slot.args) > 0 && length(slot.args) <= 4
                for slotDef in slot.args
                    if isa(slotDef, Expr) && slotDef.head == :(=) && length(slotDef.args) == 2
                        if slotDef.args[1] in [:reader, :writer, :initform]
                            setproperty!(directSlots[i], slotDef.args[1], slotDef.args[2])
                        else 
                            directSlots[i].name = slotDef.args[1]
                            directSlots[i].slotValue = slotDef.args[2]
                        end
                    else directSlots[i].name = slotDef end
                end
            end
        end
    end
    return directSlots
end

function parseDirectSuperclasses(direct_superclasses)
    supers = direct_superclasses.args
    if all(super -> isMetaobject(eval(super)), supers) supers = vcat(supers, [:Object])
    else supers = vcat(supers, [:Top]) end
    return supers
end

function compute_slot_reader_expr(method, spec)
    splitStr = split(string(method), "_")
    return quote 
        $(method)(o::$(class_name(spec))) = o.$(Symbol(splitStr[length(splitStr)]))
    end
end

function compute_slot_writer_expr(method, spec)
    splitStr = split(string(method), "_")
    return quote 
        $(method)(o::$(class_name(spec)), v) = o.$(Symbol(chop(splitStr[length(splitStr)], head=0, tail=1))) = v
    end
end

function create_generic_function(name, lambdaList)
    if !isdefined(Main, name)
        return quote
            global $(name) = Metaobject([
                $(:GenericFunction), # class_of
                $(QuoteNode(name)), # name
                $(lambdaList), # lambda-list
                [] # methods
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
    specializers = [eval(_expr.args[2]) for _expr in expr.args[1].args[2:end] if isa(_expr, Expr)]
    procedure = expr.args[2]
    return quote
        $(create_generic_function(name, lambda_list))
        push!($(name)[4], Metaobject([
            $(:MultiMethod), # class_of
            $((lambda_list...,)), # lambda_list
            $(specializers), # specializers
            (($(lambda_list...),) -> $procedure), # procedure
            [], # environment
            $(name) # generic_function
        ]))
    end
end

macro defmethod(expr)
    create_method(expr)
end

function defineClassOptionsMethods(parsedDirectSlots, spec)
    for slot in parsedDirectSlots
        if !ismissing(slot.reader)
            eval(create_method(compute_slot_reader_expr(slot.reader, spec).args[2]))
        end
        if !ismissing(slot.writer) 
            eval(create_method(compute_slot_writer_expr(slot.writer, spec).args[2]))
        end
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

macro defclass(name, direct_superclasses, direct_slots, kwargs...)
    metaclass = parseKwargs(kwargs)
    parsedDirectSuperclasses = parseDirectSuperclasses(direct_superclasses)
    parsedDirectSlots = parseDirectSlots(direct_slots, metaclass)
    return quote
        global $(esc(name)) = Metaobject([
            $(metaclass == nothing ? Class : eval(metaclass)), # class_of
            $(QuoteNode(name)), # name
            $(map(x->eval(x), parsedDirectSuperclasses)), # direct_superclasses
            $(map(x->:($x), direct_slots.args)), # direct_slots
            [], # cpl
            [], # slots
            [], # direct_subclasses
            [], # direct_methods
        ])
        # Fill the class with its direct_slots
        defineClassOptionsMethods($parsedDirectSlots, $name)
        # Call the class's initializer# TODO
    end
end


@defclass(BuiltInClass, [Class], [])
@defclass(_String, [String], [], metaclass=BuiltInClass)
@defclass(_Bool, [Bool], [], metaclass=BuiltInClass)
@defclass(_Float32, [Float32], [], metaclass=BuiltInClass)
@defclass(_Float64, [Float64], [], metaclass=BuiltInClass)
@defclass(_BigFloat, [BigFloat], [], metaclass=BuiltInClass)
@defclass(_Int64, [Int64], [], metaclass=BuiltInClass)
@defclass(_Int128, [Int128], [], metaclass=BuiltInClass)
@defclass(_BigInt, [BigInt], [], metaclass=BuiltInClass)


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

function call_generic_function(generic_function, args...)
    applicable_methods = filter(method->isApplicable(method, args), generic_function.methods)
    if length(applicable_methods) == 0
        no_applicable_method(generic_function, args)
    else 
        specificity_sorted_methods = sort_by_specificity(applicable_methods, args)
        #println(["<$(class_name(class_of(method))) $(class_name(method.generic_function))($(join([string(class_name(specializer)) for specializer in method.specializers], ", ")))>" for method in specificity_sorted_methods])
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

@defgeneric print_object(object, io)

@defmethod print_object(object::Slot, io) =
    print(io, ":$(class_name(object))")

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




@defmethod allocate_instance(class::Class) = 
    Metaobject([
        class,
        [missing for slot in class_direct_slots(class)]...
])

@defmethod initialize(instance::Object, initargs) = begin
    for (index, value) in zip(keys(initargs), collect(values(initargs)))
        setproperty!(instance, index, value)
    end
end

@defmethod initialize(instance::Class, initargs) = begin
    for (index, value) in zip(keys(initargs), collect(values(initargs)))
        setproperty!(instance, index, value)
    end
end


new(class; initargs...) = 
    let instance = allocate_instance(class)
        initialize(instance, initargs)
        instance
    end