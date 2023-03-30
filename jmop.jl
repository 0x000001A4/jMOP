import Base: setproperty!

abstract type Top end
abstract type Object <: Top end
abstract type Class end

#@defclass(class, [superclasses], [slot1, slot2, ...]) returns a new class type
macro defclass(name, superclasses, slots, kwargs...)
    begin
        println("Executing @defclass macro with arguments: ")
        println("Name of the class: $(string(name))")
        println("Superclasses: $(string(superclasses))")
        println("Slots: $(string(slots))")
        _supers = eval(superclasses)
        _slots = eval(slots)

        append!(_supers, [Object])

        _meta = Class
        _kwargs = eval(kwargs)
        if length(kwargs) > 1
            error("There should be at most 1 kwarg with the name metaclass")
        elseif length(kwargs) == 1 && _kwargs[1].args[1] == :metaclass && typeof(_kwargs[1].args[2]) == Symbol
            _meta = eval(_kwargs[1].args[2])
        end

        # quote
            # struct $(Expr(:(<:), esc(name), Union{_supers...}))
                # $(for slot in _slots println(slot) end)
            # end
        # end
        depths = []
        for super in _supers
            
        end
        supers = Union{_supers...} # TODO
        println(supers)

        res = "mutable struct " * string(name) * " <: " * string(supers) * "\n"
        for slot in _slots res *= string(slot) * "\n" end
        res *= "end"
        expr = Meta.parse(res)
        return eval(expr)
    end
end

@defclass(ComplexNumbers, [], [real, imag])

# new(class, slot1=1, slot2=2, ...) returns new instance of class
function new(class ; kwargs...)
    to_be_exec = string(class) * "("
    if length(kwargs) != length(fieldnames(class))
        println("Error in function call \"new\": new(class, slot1, slot2, ...)")
    else
        count = 0
        for field in fieldnames(class)
            for key in keys(kwargs)
                if key == field
                    to_be_exec *= string(kwargs[key]) * ", "
                    count += 1
                end
            end
        end

        if count != length(fieldnames(class))
            println("Error in function call \"new\": wrong class slot")
        else
            to_be_exec *= ")"
            expr = Meta.parse(to_be_exec)
            return eval(expr)
        end
    end
end

# getproperty(obj, slot) returns obj.slot
function getproperty(obj, slot ; kwargs...)
    to_be_exec = string(obj) * "."
    if length(kwargs) != 0
        println("Error in function call \"getproperty\": getproperty(object, slot)")
    else
        count = 0
        for field in fieldnames(typeof(obj))
            if slot == field
                to_be_exec *= string(slot)
            else
                count += 1
            end
        end
    
        if count == length(fieldnames(typeof(obj)))
            println("Error in function call \"getproperty\": object does not have slot")
        else
            expr = Meta.parse(to_be_exec)
            return eval(expr)
        end
    end
end

# getproperty(obj, slot) returns obj.slot
function setproperty!(obj, slot, value ; kwargs...)
    to_be_exec = string(obj) * "."
    if length(kwargs) != 0
        println("Error in function call \"setproperty!\": setproperty!(object, slot, value)")
    else
        count = 0
        for field in fieldnames(typeof(obj))
            if slot == field
                to_be_exec *= string(slot) * " = " * value
            else
                count += 1
            end
        end

        if count == length(fieldnames(typeof(obj)))
            println("Error in function call \"setproperty!\": object does not have slot")
        else
            expr = Meta.parse(to_be_exec)
            return eval(expr)
        end 
    end
end