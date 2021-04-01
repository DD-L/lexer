//
// copyright (c) Deel/linan
// Home:      https://github.com/DD-L/lexer
// License:   MIT
// 
#pragma once

#include <cctype>
#include <cstring>
#include <utility>
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

    //struct ScanContext
    //{
    //    //const StrRef& script; // 脚本
    //    StrRef    script; // 脚本
    //    StrRef    var;    // 当前变量   
    //};

    class VariableAllocator;

    enum class VariableFlag
    {
        Normal,
        Mutable,
    };

    // Syntax Variable 
    class Variable
    {
    public:
        Variable() = default;
        explicit Variable(VariableFlag flag)
            : m_flag(flag)
        {}

        virtual ~Variable() {
            m_mut  = nullptr;
            m_flag = VariableFlag::Normal;
        }

        virtual bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept
        {
            (void)script; (void)offset; (void)err;
            return true;
        }

        virtual bool Action() noexcept { return true; }; // = 0; @TODO

        bool IsMutable() const
        {
            return VariableFlag::Mutable == m_flag;
        }

    protected:
         void SwapMut(Variable* newMut, Variable*& oldMut = ThreadLocalDummyVariable()) noexcept
         {
             oldMut = m_mut;
             m_mut = newMut;
         }

         static Variable*& ThreadLocalDummyVariable() noexcept
         {
             thread_local Variable* dummy = nullptr;
             return dummy;
         }

        template <class Allocator>
        void ReshapeToMut(Allocator& allocator) noexcept;

        // 只需 内置 SyntaxVar 重写 _Move 即可
        virtual Variable* _Move(VariableAllocator& allocator) noexcept
        {
            return nullptr; (void)allocator;
        }

    protected:
        friend class Lexer;

        Variable*    m_mut = nullptr;  // 用作动态绑定，仅仅被用在 mut_var 的情况；
        // 但有必须写在基类中，原因是确保其他子类对象可以被安全的重塑为 MutableVariable 对象
        VariableFlag m_flag = VariableFlag::Normal;
    }; // class Variable

    namespace traits
    {
        //template <class Syntax>
        struct AllocHelper
        {
            template <class Syntax, class Vars>
            static Variable* Alloc(Vars&, Variable* newVar) noexcept
            {
                return newVar;
            }

            template <class Syntax, class Vars, class... Args>
            static Variable* Alloc(Vars& vars, Args&&... args) noexcept
            {
                Variable* var = new (std::nothrow) Syntax(std::forward<Args>(args)...);
                if (var)
                {
                    vars.push_back(var);
                }
                return var;
            }
        };

        //struct _AllocForwardFirstArg
        //{
        //    template <class Vars>
        //    Variable* operator()(Vars&, Variable* var) const noexcept
        //    {
        //        return var;
        //    }
        //};
    } // namespace traits

    // 变量内存分配器，同时承担内存持有的职责
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
            typedef DDL_LEXER::traits::AllocHelper AllocHelper;
            return AllocHelper::Alloc<Syntax>(m_vars, std::forward<Args>(args)...);
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
        std::deque<Variable*>  m_vars;
    }; // class VariableAllocator

    namespace internal
    {
        //
        // 一种动态绑定的 Variable.
        // 其意义是尽可能的复用文法（运行期），或者前式声明（编译期）
        class MutableVariable : public Variable
        {
        public:
            explicit MutableVariable(Variable* mut = nullptr)
                : Variable(VariableFlag::Mutable)
            {
                Set(mut);
            }

            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                // 确保其他 Variable 衍生对象可以安全的重塑为 mut_var
                static_assert(sizeof(MutableVariable) == sizeof(Variable), "Error"); 

                assert(Valid());
                return m_mut->Scan(script, offset, err);
            }

            bool Valid() const
            {
                return (nullptr != m_mut);
            }

            bool Set(Variable* newVar) noexcept
            {
                m_mut = newVar;
                return Valid();
            }
        }; // class MutableVariable

        // 终止符
        class SyntaxToken : public Variable
        {
            typedef SyntaxToken _Myt;

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
        
            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxToken>(m_token);
                this->~SyntaxToken();
                return var;
            }
        private:
            StrRef   m_token;
        }; // class SyntaxToken

        // Basic Structures (syntax/sentence structure)
        class SyntaxSequence : public Variable
        {
        public:
            template <class... Args>
            explicit SyntaxSequence(Args&&... args)
                : m_sequence({ std::forward<Args>(args) ... })
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
                    if (!v->Scan(script, offset, err))
                    {
                        offset = oldOffset;
                        return false;
                    }
                }
                return true;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxSequence>(std::move(m_sequence));
                this->~SyntaxSequence();
                return var;
            }

        protected:
            std::vector<Variable*>  m_sequence;
        }; // class SyntaxSequence

        // 有序分支结构
        class SyntaxBranch : public Variable
        {
        public:
            template <class... Args>
            explicit SyntaxBranch(Args&&... args)
                : m_branchs({ std::forward<Args>(args) ... })
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

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxBranch>(std::move(m_branchs));
                this->~SyntaxBranch();
                return var;
            }

        protected:
            std::vector<Variable*>  m_branchs;
        }; // class SyntaxBranch

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
                    if (!m_var->Scan(script, offset, err))
                    {
                        offset = oldOffset;
                        return false;
                    }
                }

                for (std::size_t i = m_min; i < m_max; ++i)
                {
                    oldOffset = offset;
                    if (!m_var->Scan(script, offset, err))
                    {
                        offset = oldOffset;
                        break;
                    }
                }

                return true;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxLoop>(m_var, m_min, m_max);
                this->~SyntaxLoop();
                return var;
            }

        protected:
            Variable*   m_var = nullptr;
            std::size_t m_min = 0;
            std::size_t m_max = 0;
        }; // class SyntaxLoop

        ///////////////////////////////////////////////////////////////


        // 内建标识符
        class BuiltinIdent : public Variable
        { // [_a-zA-Z][_a-zA-Z0-9]*
        public:
            explicit BuiltinIdent(StrRef name)
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
                        || (ch >= 'a' && ch <= 'z')
                        || (ch >= 'A' && ch <= 'Z')
                        || (ch >= '0' && ch <= '9')))
                    {
                        err = m_tokenName.ToStdString() + ": Error";
                        return false;
                    }
                }

                return true;
            }

        private:
            StrRef m_tokenName;
        }; // class BuiltinIdent


        // 属性
        class BuiltinAttr : public Variable
        {
            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                return true;
            }
        };

        class BuiltinWhite : public Variable
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
        }; // class BuiltinWhite

        class BuiltinNaturalNumDec : public Variable
        { // [1-9][0-9]*|0
            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            { // 十进制自然数
                if (offset < script.len)
                {
                    if (IsZero(script[offset])) { ++offset; return true; }

                    if (Is1To9(script[offset]))
                    {
                        ++offset;

                        while (offset < script.len)
                        {
                            if (Is0To9(script[offset]))
                            {
                                ++offset;
                            }
                            else
                            {
                                break;
                            }
                        }
                        return true;
                    }
                }
                return false;
            }

            bool IsZero(char c) const { return '0' == c; }
            bool Is1To9(char c) const { return '1' <= c && c <= '9'; }
            bool Is0To9(char c) const { return IsZero(c) || Is1To9(c); }
        }; // class BuiltinNaturalNumDec

        // 内置终结符的表达方式：单引号或双引号
        class BuiltinTerminator : public Variable
        {
            // 1. 单引号不支持转义，2. 双引号支持转义
            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                // 1. 单引号： '[^']+
                // 2. 双引号： 暂不实现
                if (offset < script.len)
                {
                    if ('\'' == script[offset])
                    {
                        ++offset;
                        while (offset < script.len)
                        {
                            if ('\'' == script[offset++])
                            {
                                return true;
                            }
                        }
                    }
                }
                return false;
            }
        };
    } // namespace internal

    //namespace traits
    //{
    //    template <>
    //    struct AllocHelper<DDL_LEXER::internal::SyntaxSequence, 1u> : _AllocForwardFirstArg {};
    //
    //    template <>
    //    struct AllocHelper<DDL_LEXER::internal::SyntaxBranch, 1u> : _AllocForwardFirstArg {};
    //
    //    template <>
    //    struct AllocHelper<DDL_LEXER::internal::SyntaxLoop, 1u> : _AllocForwardFirstArg {};
    //} // namespace traits

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
        bool SetGrammar(StrRef script, const VarsTable& varsTable, std::string& err) const noexcept
        {
            err.clear();
            std::size_t offset = 0;
            return ScanScript(script, varsTable, offset, err);
        }

        const Variable* GetVariable(const std::string& varName) noexcept
        {
            auto found = m_userVariablesMap.find(varName);
            if (m_userVariablesMap.end() != found)
            {
                return found->second;
            }
            return nullptr;
        }

    private:
        //static std::size_t LocateToRoot(const StrRef& script, const StrRef& rootStr) noexcept
        //{
        //    if (rootStr.len > script.len || 0 == rootStr.len)
        //    {
        //        return 0;
        //    }
        //
        //    // .*?;?\s*$root[\s:]
        //    // State: Accept
        //    // 0. Start
        //    // 1. ;?
        //    // 2. \s*
        //    // 3. $root
        //    // 4. [\s:]
        //    // 5. End
        //
        //    SyntaxToken _0x3B(";");
        //    SyntaxLoop  _0x3BOption(&_0x3B, 0, 1u);
        //
        //    internal::BuiltinWhite _white;
        //    SyntaxLoop _whitesOption(&_white, 0, SyntaxLoop::Max);
        //     
        //    SyntaxToken _rootStr(rootStr);
        //    SyntaxBranch _last(&_white, &_0x3B);
        //
        //    SyntaxSequence state(&_0x3BOption, &_whitesOption, &_rootStr, &_last);
        //
        //    const std::size_t end = script.len;
        //    std::size_t offset    = 0;
        //
        //    std::string err;
        //    while (offset < script.len)
        //    {
        //        if (state.Scan(script, offset, err))
        //        {
        //            assert(offset > 1);
        //            return (offset - 1u >= rootStr.len) ? (offset - 1u - rootStr.len) : end;
        //        }
        //    } 
        //
        //    return end;
        //}

        bool ScanScript(const StrRef& script, const VarsTable&, 
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

        template <class T, class... Args>
        Variable* Alloc(Args&&...args) noexcept
        {
            return m_variableAllocator.Alloc<T>(std::forward<Args>(args)...);
        }

        //template <class T, class Var, class... Args>
        //Variable* PlacementAlloc(Var* var, Args&&...args) noexcept
        //{
        //    var.~Var();
        //    return new (var) T(std::forward<Args>(args)...);
        //}

        // internal: 内置语法
        bool MakeInternal() noexcept
        {
            using namespace internal;
            // ident      : ; # func  (暂时使用函数)
            Variable* ident = Alloc<BuiltinIdent>("ident");
            // head       :  ident ;
            Variable* head = Alloc<SyntaxSequence>(ident);
            // var        : ident ;
            Variable* var  = Alloc<SyntaxSequence>(ident);

            // terminator    : '.*?'  # 这里暂时只使用单引号的终结符 (内部 CFG 文法中终结符的表示)
            Variable* terminator = Alloc<BuiltinTerminator>();

            // operand 
            Variable* operand = Alloc<SyntaxBranch>(terminator, var);

            // expr : 优先级 循环 >  分支 > 序列

            Variable* $0x7B = Alloc<SyntaxToken>("{"); // 可复用
            Variable* $0x7D = Alloc<SyntaxToken>("}"); // 可复用
            Variable* $0x2C = Alloc<SyntaxToken>(","); // 可复用

            // decl expr;
            Variable* expr = Alloc<MutableVariable>(); // mut_var

            // struct_with_bracket : '(' expr ')' ;
            Variable* struct_with_bracket = Alloc<SyntaxSequence>(
                Alloc<SyntaxToken>("("), expr, Alloc<SyntaxToken>(")"));

            // struct_expr : operand | struct_with_bracket # 这里 仅能容纳 var ??? 那正则表达式怎么办 ？？？TODO,  可以使用变量！！！！
            Variable* struct_expr = Alloc<SyntaxBranch>(operand, struct_with_bracket);

            // num_dec regex : [1-9][0-9]*|0 ;
            Variable* num_dec = Alloc<BuiltinNaturalNumDec>();

            Variable* loop_n = Alloc<SyntaxSequence>($0x7B, num_dec, $0x7D);
            // loop_m_n   : '{' num_dec ',' num_dec '}';
            Variable* loop_m_n = Alloc<SyntaxSequence>($0x7B, num_dec, $0x2C, num_dec, $0x7D);
            // loop_m_max : '{' num_dec ',' '}';
            Variable* loop_m_max = Alloc<SyntaxSequence>($0x7B, num_dec, $0x2C, $0x7D);

            // loop_symbol     : '?' | '*' | "+" | loop_n | loop_m_n | loop_m_max ;
            Variable* loop_symbol = Alloc<SyntaxBranch>(
                Alloc<SyntaxToken>("?"), Alloc<SyntaxToken>("*"), Alloc<SyntaxToken>("+"),
                loop_n, loop_m_n, loop_m_max);
            // loop_symbol_opt : loop_symbol ?
            Variable* loop_symbol_opt = Alloc<SyntaxLoop>(loop_symbol, 0, 1u);
            // loop_expr : struct_expr loop_symbol_opt;
            Variable* loop_expr = Alloc<SyntaxSequence>(struct_expr, loop_symbol_opt);

            // branch_expr      : '|' loop_expr  ;
            Variable* branch_expr = Alloc<SyntaxSequence>(Alloc<SyntaxToken>("|"), loop_expr);
            // branch_expr_some : branch_expr * ;
            Variable* branch_expr_some = Alloc<SyntaxLoop>(branch_expr, 0, SyntaxLoop::Max);
            // branch_pairs : loop_expr branch_expr_some ;
            Variable* branch_pairs = Alloc<SyntaxSequence>(loop_expr, branch_expr_some);

            // seq_expr  : branch_pairs + ;
            Variable* seq_expr = Alloc<SyntaxLoop>(branch_pairs, 1u, SyntaxLoop::Max);

            // expr      : seq_expr ; 
            assert(expr->IsMutable());
            expr->SwapMut(seq_expr);

            // defaultBody: ':' expr;
            Variable* defaultBody = Alloc<SyntaxSequence>(Alloc<SyntaxToken>(":"), expr);
            
            // attr      : ; # func  # func: 'regex' | 其他自定义白字符策略变量
            Variable* attr = Alloc<internal::BuiltinAttr>();

            // attrBody:    attr defaultBody;
            Variable* attrBody  = Alloc<SyntaxSequence>(attr, defaultBody);

            // body: defaultBody | attrBody;
            Variable* body  = Alloc<SyntaxBranch>(defaultBody, attrBody);

            // production :  head body ';' ; 
            Variable* production = Alloc<SyntaxSequence>(head, body, Alloc<SyntaxToken>(";"));

            // root:         production + ;
            m_internalRoot = Alloc<SyntaxLoop>(production, 1u, SyntaxLoop::Max);
            return true;
        }

    private:
        Variable*                                  m_internalRoot;
        VariableAllocator                          m_variableAllocator;
        std::unordered_map<std::string, Variable*> m_userVariablesMap;
    }; // class Lexer


    /*
char_1_9 : '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
char_0_9 : '0' | char_1_9 ;

num  :  char_1_9 char_0_9* | '0' ;


loop_suffix_n_m   : '{' num ',' num '}' ;
loop_suffix_n     : '{' num '}'         ;
loop_suffix_n_max : '{' num ',' '}'     ;

loop_suffix : '?' | '*' | '+' | loop_suffix_n_m | loop_suffix_n  | loop_suffix_n_max ;


decl  branch;

expr   : ident | ( branch )  ;

loop   :  expr loop_suffix ? ; 
seq    :  loop +             ;
branch_vec : ( '|' seq )*    ; 
branch :  seq  branch_vec    ;
    
    */

    //////////////////////////////////////////////
    // Impl

    template <class Allocator>
    void Variable::ReshapeToMut(Allocator& allocator) noexcept
    {
        if (!IsMutable())
        {
            Variable* var = _Move(allocator);

            new (this) internal::MutableVariable();
            this->m_mut = var;
        }
    }


    //template <class Syntax, class... Args>
    //Variable* VariableAllocator::Alloc(Args&&... args) noexcept
    //{
    //    return traits::AllocHelper<Syntax>::Alloc(m_vars, std::forward<Args>(args)...);
    //}
} // namespace DDL_LEXER