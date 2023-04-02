import Base: ==
import Base.show
using DataStructures

function isMetaobject(object)
    # Might need 1 more class_of
    if isa(object, Vector{Any})
        if length(object) == 0 || !isa(class_of(object), Vector{Any}) return false end
        return class_of(class_of(object)) === Class
    end
    return false
end

function compareMetaobjects(metaobject1::Vector{Any}, metaobject2::Vector{Any}, compared=Set{Tuple{Vector{Any},Vector{Any}}})
    print("m1: $metaobject1, m2: $metaobject2")

    if metaobject1 === metaobject2 return true end
    if length(metaobject1) != length(metaobject2) return false end

    if (metaobject1, metaobject2) in compared return true end
    push!(compared, (metaobject1, metaobject2))
    for i in 1:length(metaobject1)
        if isMetaobject(metaobject1[i]) && isMetaobject(metaobject2[i])
            if !compareMetaobjects(metaobject1[i], metaobject2[i], compared)
                return false
            end
        elseif metaobject1[i] != metaobject[2]
            return false
        end
    end
    return true
end

function ==(metaobject1::Vector, metaobject2::Vector)
    # In order to avoid infinite recursion
    if isMetaobject(metaobject1) && isMetaobject(metaobject2)
        return compareMetaobjects(metaobject1, metaobject2)
    end
    return isequal(metaobject1,metaobject2)
end

Class = [
    nothing, # class_of
    :Class, # name
    [], # direct_superclasses
    [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods], # direct_slots
    [], # cpl
    [:name, :direct_superclasses, :direct_slots, :cpl, :slots, :direct_subclasses, :direct_methods], # slots
    [], # direct_subclasses
    [] # direct_methods
]

Class[1] = Class
Class[5] = [Class]

Top = [Class, :Top, [], [], [Class], [], [], []]
Object = [Class, :Object, [Top], [], [Class], [], [], []]
Class[3] = [Object]


macro defclass(name, direct_superclasses, direct_slots)
    return quote
        global $(esc(name)) = [
            Class, # class_of
            $(QuoteNode(name)), # name
            vcat($(esc(direct_superclasses)), [Object]), # direct_superclasses
            $(esc(direct_slots)), # direct_slots
            [], # cpl
            $(esc(direct_slots)), # slots
            [], # direct_subclasses
            [], # direct_methods
        ]
    end
end

@defclass(GenericFunction, [], [:name, :lambda_list, :methods])
@defclass(MultiMethod, [], [:lambda_list, :specializers, :procedure, :env, :generic_function])

macro defgeneric(expr)
    dump(expr)
    name = expr.args[1]
    lambdaList = expr.args[2:end]
    return quote
        global $(esc(name)) = [
            GenericFunction, # class_of
            $(QuoteNode(name)), # name
            $(esc(lambdaList)), # lambda-list
            [], # methods
        ]
    end
end


macro defmethod(expr)
    name = expr.args[1].args[1]
    lambda_list = [_expr.args[1] for _expr in expr.args[1].args[2:end]]
    specializers = [eval(_expr.args[2]) for _expr in expr.args[1].args[2:end]]
    procedure = expr.args[2]

    return quote
        if !isdefined(Main, $(QuoteNode(name)))
            @defgeneric $(name)($(lambda_list...))
        end

        push!($(name)[4], [
            MultiMethod, # class_of
            $(esc(lambda_list)), # lambda_list
            $(esc(specializers)), # specializers
            $(QuoteNode(procedure)), # procedure
            [], # environment
            $(esc(name)) # generic_function
        ])
    end
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
        push!(cpl, super)
    end
    return cpl
end

function Base.getproperty(instance::Vector, property::Symbol)
    # instance.property resolves to:
    # -> instance asking its class for the index of the property in the slots
    # -> using this index to access its slots (+1 because of the first slots representing the class_of)
    if length(instance) > 0 && isMetaobject(instance)
        instance[1+findall(slot -> slot == property, class_of(instance)[6])[1]]
    end
end

function Base.setproperty!(instance::Vector, property::Symbol, value)
    # instance.property = value resolves to:
    # -> instance asking its class for the index of the property in the slots
    # -> using this index to access its slots (+1 because of the first slots representing the class_of)
    # -> pushing a value
    if length(instance) > 0 && isMetaobject(instance)
        instance[1+findall(slot -> slot == property, class_of(instance)[6])[1]] = value
    end
end

# Should specialize for Objects
function print_object(obj, io)
    print(io, "<$(class_name(class_of(obj))) $(string(objectid(obj), base=62))>")
end

function Base.show(io::IO, ::MIME"text/plain", object::Vector)
    if (isMetaobject(object))
        print_object(object, io)
    else 
        show(io, object)
    end
end

# Introspection
class_of(instance) = instance[1]
class_name(instance) = instance.name
class_direct_slots(instance) = instance.direct_slots
class_slots(instance) = instance.slots
class_direct_superclasses(instance) = instance.direct_superclasses
class_cpl(instance) = instance.cpl
class_direct_methods(instance) = instance.direct_methods

generic_methods(generic_function) = generic_function.methods
method_specializers(method) = method.specializers