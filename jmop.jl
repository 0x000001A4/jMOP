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

function print_method(class, io)
    print(io, "<$(class_name(class_of(class))) $(class_name(class.generic_function))($(join([string(lambda, "::", class_name(specializer)) for (lambda,specializer) in zip(class.lambda_list, class.specializers)], ", ")))>")
end

function print_function(class, io)
    print(io, "<$(class_name(class_of(class))) $(class_name(class)) with $(length(generic_methods(class))) $(length(generic_methods(class)) == 1 ? "method" : "methods")>")
end

function print_class(class, io)
    print(io, "<$(class_name(class_of(class))) $(class_name(class))>")
end

function print_object(obj, io)
    print(io, "<$(class_name(class_of(obj))) $(string(objectid(obj), base=62))>")
end

function Base.show(io::IO, ::MIME"text/plain", metaobject::Metaobject)
    if class_of(metaobject) == Class
        print_class(metaobject, io)
    elseif class_of(metaobject) == Object
        print_object(metaobject, io)
    elseif class_of(metaobject) == GenericFunction
        print_function(metaobject, io)
    elseif class_of(metaobject) == MultiMethod
        print_method(metaobject, io)
    end
end

macro defclass(name, direct_superclasses, direct_slots)
    sym_slots = []
    for slot in eval(direct_slots)
        append!(sym_slots, [Symbol(slot)])
    end
    return quote
        global $(esc(name)) = $(esc(Metaobject))([
            Class, # class_of
            $(QuoteNode(name)), # name
            vcat($(esc(direct_superclasses)), [$(esc(Object))]), # direct_superclasses
            $(esc(sym_slots)), # direct_slots
            [$(esc(Class))], # cpl
            $(esc(sym_slots)), # slots
            [], # direct_subclasses
            [], # direct_methods
        ])
    end
end

@defclass(GenericFunction, [], [:name, :lambda_list, :methods])
@defclass(MultiMethod, [], [:lambda_list, :specializers, :procedure, :env, :generic_function])

function function_dispatch(name::Metaobject, lambdaList...)
    println("function_dispatch")
    println(lambdaList)
end


function create_generic_function(name, lambdaList)
    if !isdefined(Main, name)
        func_decl = Expr(:call, Expr(:(::), esc(name), :Metaobject), esc.(lambdaList)...)
        procedure = Expr(:block, Expr(:call, :function_dispatch, esc(name), esc.(lambdaList)...))
        func_def = Expr(:function, func_decl, procedure)

        quote
            global $(esc(name)) = $(esc(Metaobject))([
                GenericFunction, # class_of
                $(QuoteNode(name)), # name
                $(esc(lambdaList)), # lambda-list
                [], # methods
            ])

            $(func_def)
        end
    end
end

macro defgeneric(expr)
    name = expr.args[1]
    lambdaList = expr.args[2:end]
    create_generic_function(name, lambdaList)
end


macro defmethod(expr)
    name = expr.args[1].args[1]
    lambda_list = [_expr.args[1] for _expr in expr.args[1].args[2:end]]
    specializers = [eval(_expr.args[2]) for _expr in expr.args[1].args[2:end]]
    procedure = expr.args[2]

    return quote
        $(create_generic_function(name, lambda_list))

        push!($(esc(name))[4], Metaobject([
            MultiMethod, # class_of
            $(esc(lambda_list)), # lambda_list
            $(esc(specializers)), # specializers
            $(QuoteNode(procedure)), # procedure
            [], # environment
            $(esc(name)) # generic_function
        ]))
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
        if !(super in cpl)
            push!(cpl, super)
        end
    end
    return cpl
end

function new(class ; kwargs...)
    instance = []
    append!(instance, [class])
    slots = []
    for slot in class[6]
        println(slot)
        sym = Symbol(slot)
        if sym in keys(kwargs)
            append!(slots, [kwargs[sym]])
        else
            append!(slots, [Nothing]) # non initialized slot            
        end
    end
    append!(instance, slots)
    return Metaobject(instance)
    # instance = [
    #    class,
    #    slot1_val,
    #    slot2_val,
    #    ...
    # ]
end
