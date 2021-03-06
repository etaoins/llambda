#ifndef _LLIBY_PORT_ABSTRACTPORT_H
#define _LLIBY_PORT_ABSTRACTPORT_H

#include <iostream>

namespace lliby
{

/**
 * Interface for Scheme port functionality using C++ virtual dispatch
 *
 * Actual input an output is accomplished used std::ostream and std::iostream. Additional functions exist to support
 * port operations with no universal equivalent in iostream such closing the standard input or output.
 */
class AbstractPort
{
public:
	virtual ~AbstractPort()
	{
	}

	/**
	 * Returns true if this is an input port
	 *
	 * This corresponds to (input-port?) in Scheme
	 */
	virtual bool isInputPort() const = 0;

	/**
	 * Returns true if this port is open for input
	 *
	 * This corresponds to (input-port-open?) in Scheme
	 */
	virtual bool isInputPortOpen() const = 0;

	/**
	 * Closes this port for input
	 *
	 * This corresponds to (close-input-port) in Scheme. Calling this on an already closed port should have no effect.
	 */
	virtual void closeInputPort() = 0;

	/**
	 * Returns true if there are bytes available to be read or the end of file has been reached
	 *
	 * This corresponds to (u8-ready?) in Scheme
	 */
	virtual bool bytesAvailable() const
	{
		return true;
	}

	/**
	 * Returns std::istream used for input from this port
	 *
	 * If the port is closed or not an input port the result of this function is undefined
	 */
	virtual std::istream *inputStream() = 0;

	/**
	 * Returns true if this is an output port
	 *
	 * This corresponds to (output-port?) in Scheme
	 */
	virtual bool isOutputPort() const = 0;

	/**
	 * Returns true if this port is open for output
	 *
	 * This corresponds to (output-port-open?) in Scheme
	 */
	virtual bool isOutputPortOpen() const = 0;

	/**
	 * Closes this port for output
	 *
	 * This corresponds to (close-output-port) in Scheme. Calling this on an already closed port should have no effect.
	 */
	virtual void closeOutputPort() = 0;

	/**
	 * Returns std::ostream used for output from this port
	 *
	 * If the port is closed or not an output port the result of this function is undefined
	 */
	virtual std::ostream *outputStream() = 0;

	/**
	 * Closes this port for input and output
	 *
	 * This corresponds to (close-port) in Scheme. The default implementation is defined in terms of closeInputPort()
	 * and closeOutputPort().
	 */
	virtual void closePort()
	{
		if (isInputPortOpen())
		{
			closeInputPort();
		}

		if (isOutputPortOpen())
		{
			closeOutputPort();
		}
	}
};

/**
 * Base class for ports only supporting output
 *
 * This mostly consists of stubs for input-related functions
 */
class AbstractOutputOnlyPort : public AbstractPort
{
	bool isInputPort() const override
	{
		return false;
	}

	bool isInputPortOpen() const override
	{
		return false;
	}

	void closeInputPort() override
	{
	}

	std::istream *inputStream() override
	{
		return nullptr;
	}

	bool isOutputPort() const override
	{
		return true;
	}
};

/**
 * Base class for ports only supporting input
 *
 * This mostly consists of stubs for output-related functions
 */
class AbstractInputOnlyPort : public AbstractPort
{
	bool isInputPort() const override
	{
		return true;
	}

	bool isOutputPort() const override
	{
		return false;
	}

	bool isOutputPortOpen() const override
	{
		return false;
	}

	void closeOutputPort() override
	{
	}

	std::ostream *outputStream() override
	{
		return nullptr;
	}
};

}

#endif
