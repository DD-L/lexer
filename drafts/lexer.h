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


    class VariableAllocator;

    enum class VariableFlag
    {
        Normal,
        Mutable,
    };

    class Action;
    typedef std::vector<Action*> ActionQueue;

    // Syntax Variable 
    class Variable
    {
    public:
        Variable() = default;
        explicit Variable(VariableFlag flag)
            : m_flag(flag)
        {}

        virtual ~Variable() {
            m_mut = nullptr;
            m_flag = VariableFlag::Normal;
        }

        virtual bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept
        {
            (void)script; (void)offset; (void)actions, (void)err;
            return true;
        }

        bool IsMutable() const
        {
            return VariableFlag::Mutable == m_flag;
        }

    protected:
         void SetName(StrRef name) noexcept
         {
             m_varName = name;
         }

         const StrRef& Name() const
         {
             return m_varName;
         }

         void SwapMut(Variable* newMut, Variable*& oldMut = ThreadLocalDummyVariable()) noexcept
         {
             oldMut = m_mut;
             m_mut = newMut;
         }

         void PushAction(ActionQueue& actions) const noexcept
         {
             if (m_action)
             {
                 actions.push_back(m_action);
             }
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

        Variable*            m_mut = nullptr;  // 用作动态绑定，仅仅被用在 mut_var 的情况；
        // 但有必须写在基类中，原因是确保其他子类对象可以被安全的重塑为 MutableVariable 对象
        VariableFlag         m_flag = VariableFlag::Normal;
        StrRef               m_varName;
        DDL_LEXER::Action*   m_action = nullptr;
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


    //    // class Action
    //class Action
    //{
    //public:
    //
    //}; // class Action
    //
    //
    //class ActionAllocator
    //{
    //public:
    //    ~ActionAllocator() noexcept
    //    {
    //        Destory();
    //    }
    //
    //    template <class ActionType>
    //    bool Alloc(const std::vector<Variable*>& vars)
    //    {
    //        Action* action = new ActionType();
    //        m_actios.push_back(action);
    //        for (Variable* v : vars)
    //        {
    //            v->SetAction(action);
    //        }
    //    } // Alloc
    //
    //private:
    //    void Destory() noexcept
    //    {
    //        for (Action* a : m_actios)
    //        {
    //            delete a;
    //        }
    //        m_actios.clear();
    //    }
    //
    //private:
    //    std::deque<Action*> m_actios;
    //}; // class ActionAllocator

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

            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept override
            {
                // 确保其他 Variable 衍生对象可以安全的重塑为 mut_var
                static_assert(sizeof(MutableVariable) == sizeof(Variable), "Error"); 

                assert(Valid());
                if (m_mut->Scan(script, offset, actions, err))
                {
                    PushAction(actions);
                    return true;
                }

                return false;
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

        // 终止符, 支持空串
        class SyntaxToken : public Variable
        {
            typedef SyntaxToken _Myt;

        public:
            SyntaxToken() = default;

            SyntaxToken(StrRef token)
                : m_token(token)
            {}

            [[nodiscard]] bool SetLeftWhites(Variable* whites) noexcept
            { // 要求：当前 Token 被当前设定的白字符从左到右 Scan，不能出现被消耗字符的情况
                std::size_t offset = 0;
                if (whites)
                { 
                    m_leftWhites = nullptr;

                    std::string e;
                    ActionQueue aq;
                    whites->Scan(m_token, offset, aq, e); (void)aq, (void)e;
                }

                m_leftWhites = whites;
                return (0 == offset);
            }

            bool SetRightAssert(Variable* _Assert) noexcept
            {
                m_rightAssert = _Assert;
                return true;
            }

            virtual bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string& err) noexcept
            {
                std::size_t oldOffset = offset;
                if (m_leftWhites)
                {
                    std::string e;
                    ActionQueue aq;
                    m_leftWhites->Scan(script, offset, aq, e); (void)aq, (void)e;
                }

                if (ScanImpl(script, offset, err))
                {
                    if (nullptr == m_rightAssert)
                    {
                        return true;
                    }
                    // 对 Token 右边界的零宽断言
                    std::size_t constOffset = offset;
                    std::string e; 
                    ActionQueue aq;
                    (void)(constOffset); (void)(e); (void)aq;
                    if (m_rightAssert->Scan(script, constOffset, aq, e))
                    {
                        return true;
                    }
                }
                offset = oldOffset; // 恢复前面白字符的消耗 ？？ @TODO 有必要 ？？？？
                return false;
            }
        
            virtual bool ScanImpl(const StrRef& script, std::size_t& offset, std::string& err) noexcept
            {
                if ((offset + m_token.len) <= script.len)
                {
                    if (0 == std::memcmp(script + offset, m_token.str, m_token.len))
                    {
                        offset += m_token.len;
                        return true;
                    }
                }

                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxToken>(m_token);
                this->~SyntaxToken();
                return var;
            }
        private:
            StrRef    m_token;
            Variable* m_leftWhites   = nullptr;
            Variable* m_rightAssert  = nullptr;
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

            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept override
            {
                std::size_t oldOffset = offset;
                std::size_t cnt = 0;
                for (Variable* v : m_sequence)
                {
                    if (!v->Scan(script, offset, actions, err))
                    {
                        offset = oldOffset;
                        return false;
                    }
                    ++cnt;
                }

                if (m_sequence.size() == cnt)
                {
                    PushAction(actions);
                    return true;
                }
                offset = oldOffset;
                return false;
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
                : m_branches({ std::forward<Args>(args) ... })
            {}

            explicit SyntaxBranch(std::vector<Variable*>&& branch)
                : m_branches(std::move(branch))
            {}

            void AppendVariable(Variable* var)
            {
                m_branches.push_back(var);
            }

            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept override
            {
                for (Variable* v : m_branches)
                {
                    const std::size_t oldQueSize = actions.size();
                    if (v->Scan(script, offset, actions, err))
                    {
                        PushAction(actions);
                        return true;
                    }
                    else
                    {
                        actions.resize(oldQueSize);
                    }
                }

                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxBranch>(std::move(m_branches));
                this->~SyntaxBranch();
                return var;
            }

        protected:
            std::vector<Variable*>  m_branches;
        }; // class SyntaxBranch

        class SyntaxLoop : public Variable
        {
        public:
            enum : std::size_t
            {
                Max = ~std::size_t(0)
            };

        public:
            SyntaxLoop(Variable* var, std::size_t min, std::size_t max)
                : m_var(var), m_min(min), m_max(max)
            {}

            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept override
            {
                std::size_t oldOffset = offset;
                std::size_t cnt = 0;
                for (std::size_t i = 0; i < m_min; ++i, ++cnt)
                { // 最少循环 m_min 次
                    if (!m_var->Scan(script, offset, actions, err))
                    {
                        offset = oldOffset;
                        return false;
                    }
                }

                for (std::size_t i = m_min; i < m_max; ++i, ++cnt)
                {
                    oldOffset = offset;
                    if (! m_var->Scan(script, offset, actions, err))
                    {
                        offset = oldOffset;
                        break;
                    }
                }

                if ((m_min <= cnt) && (cnt <= m_max))
                {
                    PushAction(actions);
                    return true;
                }
                return false;
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
        class BuiltinIdent : public SyntaxToken
        { // [_a-zA-Z][_a-zA-Z0-9]*
        public:
            using SyntaxToken::SyntaxToken;

        private:
            bool ScanImpl(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                // [_a-z-A-Z][_0-9a-zA-Z]*
                // 最小要求是一个字符
                std::size_t curOffset = offset;
                if (curOffset >= script.len)
                {
                    err = Name().ToStdString() + ": atleast one bytes.....";
                    return false;
                }

                const char first = script[curOffset++];

                if (!(('_' == first) 
                    || (first >= 'a' && first <= 'z')
                    || (first >= 'A' && first <= 'Z')))
                {
                    return false;
                }

                for (; curOffset < script.len; ++curOffset)
                {
                    const char ch = script[curOffset];
                    if (!('_' == ch
                        || (ch >= 'a' && ch <= 'z')
                        || (ch >= 'A' && ch <= 'Z')
                        || (ch >= '0' && ch <= '9')))
                    {
                        //err = Name().ToStdString() + ": Error";
                        //return false;
                        break;
                    }
                }

                offset = curOffset;
                return true;
            }
        }; // class BuiltinIdent

        class BuiltinWhite : public Variable
        {
            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string& err) noexcept override
            {
                if (offset < script.len && IsWhite(script[offset]))
                {
                    ++offset;
                    return true;
                }
                return false;
            }

        public:
            static bool IsWhite(char c) noexcept
            { // \t \n \v \f \r ' '
                switch (c)
                {
                case '\x09': case '\x0a': case '\x0b':
                case '\x0c': case '\x0d': case '\x20':
                    return true;
                default:
                    return false;
                }
            }
        }; // class BuiltinWhite

        class BuildinComment : public Variable
        { // 注释, 目前仅支持 #.*?(\n|$)
        public:
            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string&) noexcept override
            {
                if (offset < script.len)
                {
                    if ('#' == script[offset])
                    {
                        ++offset;
                        while (offset < script.len)
                        {
                            if ('\n' == script[offset])
                            {
                                ++offset;
                                return true;
                            }
                            ++offset;
                        }

                        return true;
                    }
                }
                return false;
            }
        }; // class BuildinComment

        class BuiltinPunct : public Variable
        { // ispunct : !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~ 
            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string& err) noexcept override
            {
                if (offset < script.len)
                {
                    return IsPunct(script[offset]);
                }

                return false;
            }
        public:
            static bool IsPunct(char c) noexcept
            {
                return !!std::ispunct((uint8_t)(c));
            }
        }; // class BuiltinPunct

        class BuiltinTokenRightZeroWidthAssertion : public Variable
        { // Token 右边界的零宽断言
            // $|\s|[\x21–\x2F\x3A–\x40\x5B–\x60\x7B–\x7E]
            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string& err) noexcept override
            {
                const std::size_t _offset = offset; // 不消耗字符
                if (_offset == script.len)
                {
                    return true;
                }

                if (_offset < script.len)
                {
                    char c = script[_offset];
                    return BuiltinWhite::IsWhite(c) || BuiltinPunct::IsPunct(c);
                }

                return false;
            }
        };

        class BuiltinNaturalNumDec : public SyntaxToken
        { // [1-9][0-9]*|0
        public:
            using SyntaxToken::SyntaxToken;

            bool ScanImpl(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            { // 十进制自然数
                std::size_t curOffset = offset;
                if (curOffset < script.len)
                {
                    if (IsZero(script[curOffset])) 
                    { 
                        ++curOffset; 
                        offset = curOffset; 
                        return true; 
                    }

                    if (Is1To9(script[curOffset]))
                    {
                        ++curOffset;

                        while (curOffset < script.len)
                        {
                            if (Is0To9(script[curOffset]))
                            {
                                ++curOffset;
                            }
                            else
                            {
                                break;
                            }
                        }
                        offset = curOffset;
                        return true;
                    }
                }
                return false;
            }

            bool IsZero(char c) const { return '0' == c; }
            bool Is1To9(char c) const { return '1' <= c && c <= '9'; }
            bool Is0To9(char c) const { return IsZero(c) || Is1To9(c); }
        }; // class BuiltinNaturalNumDec

        // 内置终结符的表达方式：单引号，双引号和//串
        class BuiltinTerminator : public SyntaxToken
        {
        public:
            using SyntaxToken::SyntaxToken;

            // 1. 单引号不支持转义，2. 双引号支持转义 3. 支持正则表达式
            bool ScanImpl(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                // 1. 单引号：      '[^']+
                // 2. 双引号：      暂不实现
                // 3. 正则表达式：  /.../ 支持空串
                std::size_t curOffset = offset;
                if (curOffset < script.len)
                {
                    if ('\'' == script[curOffset])
                    {
                        ++curOffset;
                        while (curOffset < script.len)
                        {
                            if ('\'' == script[curOffset++])
                            {
                                offset = curOffset;
                                return true;
                            }
                        }
                    }
                    else if ('/' == script[curOffset])
                    { // 正则表达式， 不能出现不打印字符
                        ++curOffset;
                        while (curOffset < script.len)
                        {
                            if (! std::isprint((uint8_t)(script[curOffset])))
                            {
                                err = "Unprintable characters cannot appear in regex"; // offset 也要带出去
                                return false;
                            }
                            
                            if ('\\' == script[curOffset])
                            { // 转义符跳过
                                ++curOffset;
                            }
                            if ('/' == script[curOffset++])
                            {
                                offset = curOffset;
                                return true;
                            }
                        } // end while (curOffset < script.len)
                    } // end if '/'
                }
                return false;
            }
        }; // class BuiltinTerminator
     
        ////////////////////////////////////
        //// 内置语法 Action
        //class BuiltinActionProduction
        //{
        //public:
        //
        //}; // class BuiltinActionProduction


 } // namespace internal

    // class Lexer
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
        bool ScanScript(const StrRef& script, const VarsTable&, 
            std::size_t offset, std::string& err) const noexcept
        {
            //std::vector<StrRef> tokenStream; // TokenStream
            ActionQueue aq;

            if (m_internalRoot->Scan(script, offset, aq, err))
            {
                return script.len == offset;
            }

            return false;
        }

    private:
        template <class T, class... Args,
           class = typename std::enable_if<!std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* Alloc(StrRef name, Args&&...args) noexcept
        {
            Variable* ret = m_variableAllocator.Alloc<T>(std::forward<Args>(args)...);
            if (nullptr != ret)
            {
                ret->SetName(name);
            }
            return ret;
        }

        template <class T, 
            class = typename std::enable_if<std::is_base_of<Variable, T>::value>::type>
        Variable* Alloc(Variable* var) noexcept
        {
            return m_variableAllocator.Alloc<T>(var);
        }

        template <class T, 
            class = typename std::enable_if<std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* Alloc(StrRef name, StrRef token, 
            Variable* whites = nullptr, Variable* rightAssert = nullptr) noexcept
        {
            internal::SyntaxToken* ret 
                = (internal::SyntaxToken*)m_variableAllocator.Alloc<T>(token);
            if (nullptr != ret)
            {
                ret->SetName(name);
                if (! ret->SetLeftWhites(whites))
                {
                    return nullptr;
                }

                ret->SetRightAssert(rightAssert);
            }
            return ret;
        }

        bool MakeInteranlSyntax() noexcept
        {
            using namespace internal;

            Variable* white = Alloc<BuiltinWhite>("white");
            Variable* comment = Alloc<BuildinComment>("comment");
            Variable* whites = Alloc<SyntaxLoop>("whites",
                Alloc<SyntaxBranch>("white_or_comment", white, comment), 0, SyntaxLoop::Max);
            Variable* tokenRightAssert = Alloc<BuiltinTokenRightZeroWidthAssertion>("tokenRightAssert");

            // ident      : ; # func  (暂时使用函数)
            Variable* ident = Alloc<BuiltinIdent>("ident", "", whites); // ident 不需要右边界断言
            // head       :  ident ;
            Variable* head = Alloc<SyntaxSequence>(ident);
            // var        :  ident ;
            Variable* var = Alloc<SyntaxSequence>(ident);

            // terminator    : '.*?'  # 或者正则表达式 
            Variable* terminator = Alloc<BuiltinTerminator>("terminator", "", whites);

            // operand 
            Variable* operand = Alloc<SyntaxBranch>("operand", terminator, var);

            // expr : 优先级 循环 > 序列 > 分支
            Variable* $0x7B = Alloc<SyntaxToken>("{", "{", whites);
            Variable* $0x7D = Alloc<SyntaxToken>("}", "}", whites);
            Variable* $0x2C = Alloc<SyntaxToken>(",", ",", whites);
            Variable* $0x28 = Alloc<SyntaxToken>("(", "(", whites);
            Variable* $0x29 = Alloc<SyntaxToken>(")", ")", whites);

            // decl expr;
            Variable* expr = Alloc<MutableVariable>("expr"); // mut_var

            // struct_with_bracket : '(' expr ')' ;
            Variable* struct_with_bracket = Alloc<SyntaxSequence>("struct_with_bracket", $0x28, expr, $0x29);

            // struct_expr : operand | struct_with_bracket # 这里 仅能容纳 var ??? 那正则表达式怎么办 ？？？TODO,  可以使用变量！！！！
            Variable* struct_expr = Alloc<SyntaxBranch>("struct_expr", operand, struct_with_bracket);

            // num_dec regex : [1-9][0-9]*|0 ;
            Variable* num_dec = Alloc<BuiltinNaturalNumDec>("num_dec", "", whites, tokenRightAssert);

            Variable* loop_n = Alloc<SyntaxSequence>("loop_n", $0x7B, num_dec, $0x7D);
            // loop_m_n   : '{' num_dec ',' num_dec '}';
            Variable* loop_m_n = Alloc<SyntaxSequence>("loop_m_n",
                $0x7B, num_dec, $0x2C, num_dec, $0x7D);
            // loop_m_max : '{' num_dec ',' '}';
            Variable* loop_m_max = Alloc<SyntaxSequence>("loop_m_max", $0x7B, num_dec, $0x2C, $0x7D);

            // loop_symbol     : '?' | '*' | "+" | loop_n | loop_m_n | loop_m_max ;
            Variable* loop_symbol = Alloc<SyntaxBranch>("loop_symbol",
                Alloc<SyntaxToken>("?", "?", whites),
                Alloc<SyntaxToken>("*", "*", whites),
                Alloc<SyntaxToken>("+", "+", whites),
                loop_n, loop_m_n, loop_m_max);
            // loop_symbol_opt : loop_symbol ?
            Variable* loop_symbol_opt = Alloc<SyntaxLoop>("loop_symbol_opt", loop_symbol, 0, 1u);
            // loop_expr : struct_expr loop_symbol_opt;
            Variable* loop_expr = Alloc<SyntaxSequence>("loop_expr", struct_expr, loop_symbol_opt);

            // seq_expr : loop_expr +
            Variable* seq_expr = Alloc<SyntaxLoop>("seq_expr", loop_expr, 1u, SyntaxLoop::Max);

            // branch_expr : '|' seq_expr;
            Variable* branch_expr = Alloc<SyntaxSequence>("branch_expr", Alloc<SyntaxToken>("|", "|", whites), seq_expr);
            // seq_expr_some : branch_expr *;
            Variable* branch_expr_some = Alloc<SyntaxLoop>("branch_expr_some", branch_expr, 0, SyntaxLoop::Max);
            // branch_pairs   : branch_expr branch_expr_some
            Variable* branch_pairs = Alloc<SyntaxSequence>("branch_pairs", seq_expr, branch_expr_some);

            // expr      ： branch_expr;
            assert(expr->IsMutable());
            expr->SwapMut(branch_pairs);

            // productionStatement :  head body ';' ; 
            Variable* production_statement = Alloc<SyntaxSequence>("production_statement", head, Alloc<SyntaxToken>(":", ":", whites), expr);

            // semicolon_opt : ','?;
            Variable* semicolon_opt = Alloc<SyntaxLoop>("semicolon_opt", $0x2C, 0u, 1u);

            Variable* let_keyword = Alloc<SyntaxToken>("let_keyword", "let", whites, tokenRightAssert);
            Variable* equal_mark = Alloc<SyntaxToken>("=", "=", whites);
            Variable* where_keyword = Alloc<SyntaxToken>("where", "where", whites, tokenRightAssert);

            // operand_swap : operand '->' operand;
            Variable* operand_swap = Alloc<SyntaxSequence>("operand_swap", operand, Alloc<SyntaxToken>("->", "->", whites), operand);
            // operand_swap_some:  (semicolon_opt operand_swap)*;
            Variable* operand_swap_some = Alloc<SyntaxLoop>("operand_swap_some",
                Alloc<SyntaxSequence>("another_operand_swap", semicolon_opt, operand_swap),
                0, SyntaxLoop::Max);


            // let var = var2, where a -> b, c -> d; // @TODO 这里是否要求 a 和 c 必须是非终结符或指向终结符的变量？ !!!!!!!!!!!!!! 后者无歧义？？？
            // let_where : 'let' head '=' var ','? 'where' operand '->' operand (','? operand '->' operand)*
            //Variable* let_where = Alloc<SyntaxSequence>("let_where", let_var_clause, where_clause);
            Variable* let_where = Alloc<SyntaxSequence>("let_where", let_keyword, head, equal_mark, var, semicolon_opt,
                where_keyword, operand_swap, operand_swap_some);

            // let var = $func();
            // func_expr: 'let' var '=' /$func\s*(/ (operand(',' operand)*) ? ')';
            Variable* local_func_prefix = Alloc<SyntaxToken>("local_func_prefix", "$", whites);
            Variable* func_name = Alloc<SyntaxSequence>("func_name", local_func_prefix, ident);
            Variable* suffix_args = Alloc<SyntaxSequence>("suffix_args", $0x2C, operand);
            Variable* suffix_args_some = Alloc<SyntaxLoop>("suffix_args_some", suffix_args, 0, SyntaxLoop::Max);
            Variable* func_args = Alloc<SyntaxSequence>("func_args", operand, suffix_args_some);
            Variable* func_args_opt = Alloc<SyntaxLoop>("func_args_opt", func_args, 0, 1u);
            Variable* func_expr = Alloc<SyntaxSequence>("func_expr", func_name, $0x28, func_args_opt, $0x29);
            Variable* let_func = Alloc<SyntaxSequence>("let_func", let_keyword, head, equal_mark, func_expr);

            // statement : production_statement | let_statement ;
            Variable* statement = Alloc<SyntaxBranch>("statement", production_statement, let_func, let_where);

            // statement_with_endmark : statement ';'+;
            Variable* statement_with_endmark = Alloc<SyntaxSequence>("statement_with_endmark", statement,
                Alloc<SyntaxLoop>("statement_endmark", Alloc<SyntaxToken>(";", ";", whites), 1u, SyntaxLoop::Max), whites);

            // root:         statement_with_endmark + ;
            m_internalRoot = Alloc<SyntaxLoop>("root", statement_with_endmark, 1u, SyntaxLoop::Max);

            return true;
        } // MakeInteranlSyntax

        bool MakeInternalAction() noexcept
        {
            return true;
            ////////////////////////////////////////////////////////////////////////////////////////////
            //
            // 变量与 Action 的关系是：
            // 1. 自定义函数的情况下，变量本身就可以是 Action（函数是Action, 函数的返回值或 this 是变量），而且还可以携带用户自定义数据。
            // 2. 一个变量最多绑定一个 Action，一个 Action 可以挂在多个变量上

            //m_actionAllocator.Alloc<BuiltinActionProduction>({ production_statement }) || Error("Faild To Create Action : production_statement");
            //m_actionAllocator.Alloc<>({ let_func }) || Error("Faild To Create Action : let_func");
            //m_actionAllocator.Alloc<>({ let_where }) || Error("Faild To Create Action : let_where");

        } // MakeInternalAction

        // internal: 内置语法
        bool MakeInternal() noexcept
        {
            return MakeInteranlSyntax() && MakeInternalAction();
        }

        //static bool Error(StrRef err)
        //{
        //    throw(err.ToStdString());
        //}

    private:
        Variable*                                  m_internalRoot;
        VariableAllocator                          m_variableAllocator;
        std::unordered_map<std::string, Variable*> m_userVariablesMap;   // 输出(1) 
        //ActionAllocator                            m_actionAllocator;
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

} // namespace DDL_LEXER