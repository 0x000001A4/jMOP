abstract type Top end
abstract type Object <: Top end
abstract type Class end

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

        res = "struct " * string(name) * " <: " * string(supers) * "\n"
        for slot in _slots res *= string(slot) * "\n" end
        res *= "end"
        expr = Meta.parse(res)
        return eval(expr)
    end
end

@defclass(ComplexNumbers, [], [real, imag])

function new(class ; kwargs...)
    to_be_exec = string(class) * "("
    if length(kwargs) != length(fieldnames(class))
        println("Deu cocó")
    end

    for field in fieldnames(class)
        for key in keys(kwargs)
            if key == field
                to_be_exec *= string(kwargs[key]) * ", "
            end
        end
    end
    to_be_exec *= ")"
    expr = Meta.parse(to_be_exec)
    return eval(expr)    
end

#@defclass(ComplexNumbers, [], [real, imag])
#cn = new(ComplexNumbers, real=1, imag=2)