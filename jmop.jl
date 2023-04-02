import Base: ==
import Base.show
using DataStructures

struct Metaobject
    value::Vector{Any}
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
        instance[1+findall(slot -> slot == property, class_of(instance))[1]] = value
    end
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

function compareMetaobjects(metaobject1, metaobject2, compared=Set{Tuple{Metaobject,Metaobject}})
    if metaobject1 === metaobject2 return true end
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

function ==(metaobject1::Metaobject, metaobject2::Metaobject)
    # In order to avoid infinite recursion
    compareMetaobjects(metaobject1, metaobject2)
end

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

function print_class(class, io)
    print(io, "<$(class_name(class_of(class))) $(class_name(class))>")
end

function print_object(obj, io)
    print(io, "<$(class_name(class_of(obj))) $(string(objectid(obj), base=62))>")
end

function Base.show(io::IO, ::MIME"text/plain", metaobject::Metaobject)
    print_class(metaobject, io)
end

macro defclass(name, direct_superclasses, direct_slots)
    return quote
        global $(esc(name)) = $(esc(Metaobject))([
            Class, # class_of
            $(QuoteNode(name)), # name
            vcat($(esc(direct_superclasses)), [$(esc(Object))]), # direct_superclasses
            $(esc(direct_slots)), # direct_slots
            [$(esc(Class))], # cpl
            $(esc(direct_slots)), # slots
            [], # direct_subclasses
            [], # direct_methods
        ])
    end
end

@defclass(GenericFunction, [], [:name, :lambda_list, :methods])
@defclass(MultiMethod, [], [:lambda_list, :specializers, :procedure, :env, :generic_function])

macro defgeneric(expr)
    dump(expr)
    name = expr.args[1]
    lambdaList = expr.args[2:end]
    return quote
        global $(esc(name)) = $(esc(Metaobject))([
            GenericFunction, # class_of
            $(QuoteNode(name)), # name
            $(esc(lambdaList)), # lambda-list
            [], # methods
        ])
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
