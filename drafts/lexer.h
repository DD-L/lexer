//
// copyright (c) Deel/linan
// Home:      https://github.com/DD-L/lexer
// License:   MIT
// 
#pragma once

#include <cctype>
#include <unordered_map>
#include <functional>
#include <vector>
#include <deque>
#include <assert.h>

#ifndef DDL_LEXER
#define DDL_LEXER DDL_LEXER
#endif // DDL_LEXER

namespace DDL_LEXER
{
    class Lexer;
   
    struct StrRef
    {
        const char* str = nullptr;
        std::size_t len = 0;

    public:
        constexpr StrRef() = default;

        constexpr StrRef(const char* _str, std::size_t _len)
            : str(_str), len(_len)
        {}

        template <std::size_t N>
        constexpr StrRef(const char (&arr)[N] )
            : str(arr), len(N - 1)
        {
            static_assert(0 != N, "Error");
        }

        constexpr StrRef(const StrRef& strRef) noexcept
            : str(strRef.str), len(strRef.len)
        {}

        std::string ToStdString() const
        {
            return std::string(str, len);
        }

        const char* operator+(std::size_t offset) const noexcept
        {
            return offset < len ? str + offset : nullptr;
        }

        char operator[](std::size_t offset) const noexcept
        {
            return *((*this) + offset); // may crash ...
        }
    }; //  StrRef

    struct ScanContext
    {
        //const StrRef& script; // 脚本
        StrRef    script; // 脚本
        StrRef    var;    // 当前变量   
    };

    class Variable
    {
    public:
        Variable() = default;

        virtual ~Variable() {}

        virtual bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept
        {
            return true;
        }

        virtual bool Action() noexcept 
        {
            return true;
        }

    //protected:
    //   ScanContext   m_scanContext;
    };

    // 终止符
    class SyntaxToken : public Variable
    {
    public:
        SyntaxToken(StrRef token)
            : m_token(token)
        {}

        virtual bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept
        {
            if ((offset + m_token.len) > script.len)
            {
                return false;
            }

            return 0 == std::memcmp(script + offset, m_token.str, m_token.len);
        }

    private:
        StrRef   m_token;
    };

    // Basic Structures (syntax/sentence structure)
    class SyntaxSequence : public Variable
    {
    public:
        template <class... Args>
        explicit SyntaxSequence(Args&&... args)
            : m_sequence({ std::forward(args) ... })
        {}

        explicit SyntaxSequence(std::vector<Variable*>&& branch)
            : m_sequence(std::move(branch))
        {}

        void AppendVariable(Variable* var)
        {
            m_sequence.push_back(var);
        }

        bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
        {
            std::size_t oldOffset = offset;
            for (Variable* v : m_sequence)
            {
                if (! v->Scan(script, offset, err))
                {
                    offset = oldOffset;
                    return false;
                }
            }
            return true;
        }

    protected:
        std::vector<Variable*>  m_sequence;
    };

    // 有序分支结构
    class SyntaxBranch : public Variable
    {
    public:
        template <class... Args>
        explicit SyntaxBranch(Args&&... args)
            : m_branchs({ std::forward(args) ... })
        {}

        explicit SyntaxBranch(std::vector<Variable*>&& branch)
            : m_branchs(std::move(branch))
        {}

        void AppendVariable(Variable* var)
        {
            m_branchs.push_back(var);
        }

        bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
        {
            for (Variable* v : m_branchs)
            {
                if (v->Scan(script, offset, err))
                {
                    return true;
                }
            }

            return false;
        }

    protected:
        std::vector<Variable*>  m_branchs;
    };

    class SyntaxLoop : public Variable
    {
    public:
        static constexpr std::size_t Max = ~std::size_t(0);

    public:
        SyntaxLoop(Variable* var, std::size_t min, std::size_t max)
            : m_var(var), m_min(min), m_max(max)
        {}

        bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
        {
            std::size_t oldOffset = offset;
            for (std::size_t i = 0; i < m_min; ++i)
            { // 最少循环 m_min 次
                if (! m_var->Scan(script, offset, err))
                {
                    offset = oldOffset;
                    return false;
                }
            }

            for (std::size_t i = m_min; i < m_max; ++i)
            {
                oldOffset = offset;
                if (! m_var->Scan(script, offset, err))
                {
                    offset = oldOffset;
                    break;
                }
            }

            return true;
        }

    protected:
        Variable*   m_var = nullptr;
        std::size_t m_min = 0;
        std::size_t m_max = 0;
    };

    namespace traits
    {
        template <class Syntax, std::size_t>
        struct AllocHelper
        {
            template <class Vars, class... Args>
            Variable* operator()(Vars& vars, Args&&... args) const noexcept
            {
                Variable* var = new (std::nothrow) Syntax(std::forward<Args>(args)...);
                if (var)
                {
                    vars.push_back(var);
                }
                return var;
            }
        };

        struct _AllocForwardFirstArg
        {
            template <class Vars>
            Variable* operator()(Vars&, Variable* var) const noexcept
            {
                return var;
            }
        };

        template <>
        struct AllocHelper<SyntaxSequence, 1u> : _AllocForwardFirstArg {};

        template <>
        struct AllocHelper<SyntaxBranch, 1u> : _AllocForwardFirstArg {};

        template <>
        struct AllocHelper<SyntaxLoop, 1u> : _AllocForwardFirstArg {};
    } // namespace traits

    namespace internal
    {
        // 内建标识符
        class SyntaxIdent : public Variable
        { // [_a-zA-Z][_a-zA-Z0-9]*
        public:
            explicit SyntaxIdent(StrRef name)
                : m_tokenName(name)
            {}

        private:
            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                // [_a-z-A-Z][_0-9a-zA-Z]*
                // 最小要求是一个字符
                if (offset >= script.len)
                {
                    err = m_tokenName.ToStdString() + ": atleast one bytes.....";
                    return false;
                }

                const char first = script[offset++];

                if (!(('_' == first) 
                    || (first >= 'a' && first <= 'z')
                    || (first >= 'A' && first <= 'Z')))
                {
                    return false;
                }

                for (; offset < script.len; ++offset)
                {
                    const char ch = script[offset];
                    if (!('_' == ch
                        || ch >= 'a' && ch <= 'z'
                        || ch >= 'A' && ch <= 'Z'
                        || ch >= '0' && ch <= '9'))
                    {
                        err = m_tokenName.ToStdString() + ": Error";
                        return false;
                    }
                }

                return true;
            }

        private:
            StrRef m_tokenName;
        }; // class SyntaxIdent

        // 表达式
        class SyntaxExpr : public Variable
        {
        };

        // 属性
        class SyntaxAttr : public Variable
        {
            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                 
            }
        };

        class SyntaxWhite : public Variable
        {
            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                if (offset < script.len)
                {
                    switch (script[offset])
                    {
                    case '\x09': // \t
                    case '\x0a': // \n
                    case '\x0b': // \v
                    case '\x0c': // \f
                    case '\x0d': // \r
                    case '\x20': // ' '
                        return true;
                    }
                }
                return false;
            }
        }; // class SyntaxWhite
    } // namespace interanl

    // 
    class VariableAllocator
    {
    public:
        ~VariableAllocator() noexcept
        {
            Destroy();
        }

        template <class Syntax, class... Args>
        Variable* Alloc(Args&&... args) noexcept
        {
            traits::AllocHelper<Syntax, (sizeof...(args))> allocHelper;
            return allocHelper(m_vars, std::forward<Args>(args)...);
        }

    private:
        void Destroy() noexcept
        {
            for (Variable* v : m_vars)
            {
                delete v;
            }

            m_vars.clear();
        }
    private:
        std::deque<Variable*> m_vars;
    }; // 

    //static inline bool InternalScan(Variable* v) noexcept
    //{
    //    assert(nullptr != v);
    //
    //    v->Scan();
    //}


    class Lexer
    {
    public:
        typedef std::unordered_map<std::string, Variable*> VarsTable;

    public:
        Lexer()
        {
            MakeInternal();
        }

        VariableAllocator& GetVariableAllocator()
        {
            return m_variableAllocator;
        }

        // 线程无关
        bool SetGrammar(StrRef script, StrRef root, const VarsTable& varsTable, std::string& err) const noexcept
        {
            err.clear();
                      
            std::size_t offset = LocateToRoot(script, root);
            return ScanScript(script, varsTable, offset, err);
        }

    private:
        static std::size_t LocateToRoot(const StrRef& script, const StrRef& rootStr) noexcept
        {
            if (rootStr.len > script.len)
            {
                return 0;
            }

            // .*;?\s*$root[\s:];
            // State: Accept
            // 0. Start
            // 1. ;?
            // 2. \s*
            // 3. $root
            // 4. [\s:]
            // 5. End

            SyntaxToken _0x3B(";");
            SyntaxLoop  _0x3BOption(&_0x3B, 0, 1u);

            internal::SyntaxWhite _white;
            SyntaxLoop _whitesOption(&_white, 0, SyntaxLoop::Max);
             
            SyntaxToken _rootStr(rootStr);
            SyntaxBranch _last(&_white, &_0x3B);

            SyntaxSequence state(&_0x3BOption, &_whitesOption, &_rootStr, &_last);

            const std::size_t end = script.len;
            std::size_t offset    = 0;

            std::string err;
            while (offset < script.len)
            {
                if (state.Scan(script, offset, err))
                {
                    return (offset >= rootStr.len) ? (offset - rootStr.len) : end;
                }
            } 

            return end;
        }

        bool ScanScript(const StrRef& script, const VarsTable& varsTable, 
            std::size_t offset, std::string& err) const noexcept
        {
            std::vector<StrRef> tokenStream; // TokenStream

            if (m_internalRoot->Scan(script, offset, err))
            {
                return script.len == offset;
            }

            return false;
        }

    private:
        // internal: 内置语法
        bool MakeInternal() noexcept
        {
            // ident      : ; # func  (暂时使用函数)
            Variable* ident = m_variableAllocator.Alloc<internal::SyntaxIdent>("ident");

            // head       :  ident ;
            Variable* head = m_variableAllocator.Alloc<SyntaxSequence>(ident);

            // token_0x3A  : ':' ;
            Variable* token_0x3A = m_variableAllocator.Alloc<SyntaxToken>(":");

            // expr      : ; # func (暂时使用函数)
            Variable* expr       = m_variableAllocator.Alloc<internal::SyntaxExpr>();

            // defaultBody: token_0x3A, expr;
            Variable* defaultBody = m_variableAllocator.Alloc<SyntaxSequence>(token_0x3A, expr);
            
            // attr      : ; # func  # func: 'regex' | 其他自定义白字符策略变量
            Variable* attr = m_variableAllocator.Alloc<internal::SyntaxAttr>();

            // attrBody:    attr defaultBody;
            Variable* attrBody  = m_variableAllocator.Alloc<SyntaxSequence>(attr, defaultBody);

            // body: defaultBody | attrBody;
            Variable* body  = m_variableAllocator.Alloc<SyntaxBranch>(defaultBody, attrBody);

            // token_0x3B  : ';' ;
            Variable* token_0x3B = m_variableAllocator.Alloc<SyntaxToken>(";");

            // production :  head attrOption token_0x3A expr token_0x3B ; 
            Variable* production = m_variableAllocator.Alloc<SyntaxSequence>(head, body, token_0x3B);

            // root:         production + ;
            m_internalRoot = m_variableAllocator.Alloc<SyntaxLoop>(production, 1u, SyntaxLoop::Max);
        }

    private:
        Variable*           m_internalRoot;
        VariableAllocator   m_variableAllocator;
    }; // class Lexer



    ///////////////////////////////////

    class Header
    {};

    class Body
    {};

    class Production
    {
        Header  m_header;
        Body    m_body;
    };



} // namespace DDL_LEXER